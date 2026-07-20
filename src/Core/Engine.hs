{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mycfg.Core.Engine (
    Engine (..),
    EngineConfig (..),
    EngineState (..),
    EngineResult (..),
    EngineError (..),
    initializeEngine,
    runEngine,
    shutdownEngine,
    getEngineState,
) where

import Control.Exception (SomeException, bracket, bracketOnError, try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path, toFilePath)

import Mycfg.Config.Defaults
import Mycfg.Config.Parser
import Mycfg.Config.Types
import Mycfg.Config.Validator
import Mycfg.Core.Apply
import Mycfg.Core.Diff
import Mycfg.Core.Planner
import Mycfg.Errors.Types
import Mycfg.Logging.Events
import Mycfg.Logging.Logger
import Mycfg.State.Generations
import Mycfg.State.Store

data Engine = Engine
    { config :: EngineConfig
    , state :: EngineState
    , logger :: Logger
    , stateStore :: StateStore
    }

data EngineConfig = EngineConfig
    { verbose :: Bool
    , dryRun :: Bool
    , force :: Bool
    , backupEnabled :: Bool
    , maxRetries :: Int
    , stateDirectory :: Maybe (Path Abs Dir)
    }
    deriving (Show, Eq, Generic)

instance ToJSON EngineConfig
instance FromJSON EngineConfig

data EngineState = EngineState
    { initialized :: Bool
    , currentConfig :: Maybe Config
    , currentGeneration :: Maybe Generation
    , operationCount :: Int
    , lastOperation :: Maybe UTCTime
    }
    deriving (Show, Eq, Generic)

instance ToJSON EngineState
instance FromJSON EngineState

data EngineResult
    = EngineSuccess EngineState
    | EngineFailure EngineError
    deriving (Show, Eq)

data EngineError
    = InitializationFailed Text
    | ConfigurationError Text
    | StateError Text
    | OperationError Text
    | ValidationError Text
    deriving (Show, Eq)

initializeEngine :: EngineConfig -> Logger -> IO EngineResult
initializeEngine engineConfig logger = do
    withOperation logger $ \opCtx -> do
        logSystemStarted opCtx "0.1.0"

        storeResult <- initializeStateStore (stateDirectory engineConfig)
        case storeResult of
            Left err -> do
                logError opCtx "Failed to initialize state store" $ return ()
                return $ EngineFailure $ StateError $ Text.pack $ show err
            Right store -> do
                logInfo opCtx "State store initialized successfully" $ return ()

                currentGen <- getCurrentGeneration store
                let engineState =
                        EngineState
                            { initialized = True
                            , currentConfig = Nothing
                            , currentGeneration = currentGen
                            , operationCount = 0
                            , lastOperation = Nothing
                            }

                let engine =
                        Engine
                            { config = engineConfig
                            , state = engineState
                            , logger = logger
                            , stateStore = store
                            }

                logInfo opCtx "Engine initialized successfully" $ return ()
                return $ EngineSuccess engineState

runEngine :: Logger -> EngineConfig -> IO EngineResult
runEngine logger engineConfig = do
    initResult <- initializeEngine engineConfig logger
    case initResult of
        EngineFailure err -> return $ EngineFailure err
        EngineSuccess engineState -> do
            storeResult <- initializeStateStore (stateDirectory engineConfig)
            case storeResult of
                Left err -> return $ EngineFailure $ StateError $ Text.pack $ show err
                Right store -> do
                    let engine =
                            Engine
                                { config = engineConfig
                                , state = engineState
                                , logger = logger
                                , stateStore = store
                                }

                    withOperation logger $ \opCtx -> do
                        logInfo opCtx "Starting engine operation" $ do
                            executeEngineOperation engine

executeEngineOperation :: Engine -> IO EngineResult
executeEngineOperation engine = do
    let engineConfig = config engine
        store = stateStore engine
        logger' = logger engine

    withOperation logger' $ \opCtx -> do
        configResult <- loadDefaultConfiguration
        case configResult of
            Left err -> do
                logError opCtx "Failed to load configuration" $ return ()
                return $ EngineFailure $ ConfigurationError $ Text.pack $ show err
            Right cfg -> do
                logConfigLoaded opCtx "default" (modules cfg) $ return ()

                validationResult <- validateConfig cfg
                case validationResult of
                    ValidationFailure errors -> do
                        logValidationFailed opCtx (map Text.pack $ map show errors) $ return ()
                        return $ EngineFailure $ ValidationError $ Text.pack $ show errors
                    ValidationSuccess warnings -> do
                        logValidationCompleted opCtx (map Text.pack $ map show warnings) $ return ()

                        let applyOptions =
                                ApplyOptions
                                    { dryRun = dryRun engineConfig
                                    , force = force engineConfig
                                    , backupEnabled = backupEnabled engineConfig
                                    , validateBeforeApply = True
                                    , continueOnError = False
                                    , maxRetries = maxRetries engineConfig
                                    }

                        logApplyStarted opCtx (dryRun engineConfig) $ return ()
                        applyResult <- applyConfiguration store cfg applyOptions

                        case success applyResult of
                            True -> do
                                logApplyCompleted opCtx (appliedSteps applyResult) (dryRun engineConfig) $ return ()
                                return $ EngineSuccess $ state engine
                            False -> do
                                logApplyFailed opCtx "Apply operation failed" (failedSteps applyResult) $ return ()
                                return $ EngineFailure $ OperationError "Apply operation failed"

loadDefaultConfiguration :: IO (Either MycfgError Config)
loadDefaultConfiguration = do
    homeDir <- getHomeDirectory
    let ringRoot = homeDir </> ".saturn"
        ringConfigPath = ringRoot </> "ring.saturn"
        legacyConfigPath = homeDir </> ".config" </> "mycfg" </> "config.toml"

    ringExists <- doesFileExist (toFilePath ringConfigPath)
    if ringExists
        then do
            case parseAbsFile (toFilePath ringConfigPath) of
                Left _ -> return $ Left $ ParseError $ InvalidToml undefined "Invalid ring path"
                Right path -> do
                    parseResult <- parseConfig path
                    case parseResult of
                        Left err -> return $ Left err
                        Right cfg -> return $ Right cfg{ring = Just (fromMaybe defaultRingConfig (ring cfg))}
        else do
            legacyExists <- doesFileExist legacyConfigPath
            if legacyExists
                then do
                    case parseAbsFile legacyConfigPath of
                        Left _ -> return $ Left $ ParseError $ InvalidToml undefined "Invalid config path"
                        Right path -> parseConfig path
                else do
                    let ring = defaultRingConfig
                    return $ Right defaultConfig{ring = Just ring}

shutdownEngine :: Engine -> IO EngineResult
shutdownEngine engine = do
    let logger' = logger engine

    withOperation logger' $ \opCtx -> do
        logSystemShutdown opCtx $ return ()
        return $ EngineSuccess $ state engine

getEngineState :: Engine -> EngineState
getEngineState engine = state engine

updateEngineState :: Engine -> EngineState -> Engine
updateEngineState engine newState = engine{state = newState}

incrementOperationCount :: EngineState -> EngineState
incrementOperationCount state = state{operationCount = operationCount state + 1}

updateLastOperation :: EngineState -> UTCTime -> EngineState
updateLastOperation state time = state{lastOperation = Just time}

setCurrentConfig :: EngineState -> Config -> EngineState
setCurrentConfig state cfg = state{currentConfig = Just cfg}

setCurrentGeneration :: EngineState -> Generation -> EngineState
setCurrentGeneration state gen = state{currentGeneration = Just gen}

validateEngineConfig :: EngineConfig -> Either EngineError ()
validateEngineConfig engineConfig = do
    when (maxRetries engineConfig < 0) $
        Left $
            InitializationFailed "maxRetries must be non-negative"

    when (maxRetries engineConfig > 10) $
        Left $
            InitializationFailed "maxRetries must be <= 10"

    Right ()

validateEngineState :: EngineState -> Either EngineError ()
validateEngineState engineState = do
    unless (initialized engineState) $
        Left $
            InitializationFailed "Engine not initialized"

    Right ()

createBackup :: Engine -> IO (Either EngineError ())
createBackup engine = do
    let store = stateStore engine

    currentGen <- getCurrentGeneration store
    case currentGen of
        Just gen -> do
            snapshotId <- generateSnapshotId
            let description = "Engine backup - " ++ Text.unpack (generationId $ metadata gen)

            result <- createSnapshot snapshotId (Text.pack description) []
            case result of
                Left _ -> return $ Left $ StateError "Failed to create backup"
                Right snapshot -> do
                    let snapshotPath = snapshotsDirectory store </> Text.unpack snapshotId ++ ".json"
                    saveResult <- saveSnapshot snapshot snapshotPath
                    case saveResult of
                        Left _ -> return $ Left $ StateError "Failed to save backup"
                        Right _ -> return $ Right ()
        Nothing -> return $ Right ()

checkEngineHealth :: Engine -> IO (Either EngineError ())
checkEngineHealth engine = do
    let store = stateStore engine
        engineState = state engine

    validateResult <- validateEngineState engineState
    case validateResult of
        Left err -> return $ Left err
        Right _ -> do
            storeResult <- validateStateStore store
            case storeResult of
                Left err -> return $ Left $ StateError $ Text.pack $ show err
                Right _ -> return $ Right ()

getEngineStatistics :: Engine -> IO (Map Text Text)
getEngineStatistics engine = do
    let engineState = state engine
        store = stateStore engine

    genStats <- getGenerationStatistics store
    case genStats of
        Left _ -> return Map.empty
        Right stats -> do
            let statsMap =
                    Map.fromList
                        [ ("initialized", Text.pack $ show $ initialized engineState)
                        , ("operation_count", Text.pack $ show $ operationCount engineState)
                        ,
                            ( "current_generation"
                            , case currentGeneration engineState of
                                Just gen -> generationId $ metadata gen
                                Nothing -> "none"
                            )
                        ]
            return statsMap

cleanupEngine :: Engine -> IO (Either EngineError ())
cleanupEngine engine = do
    let store = stateStore engine
        engineConfig = config engine

    cleanupResult <- cleanupOldGenerations store 10
    case cleanupResult of
        Left err -> return $ Left $ StateError $ Text.pack $ show err
        Right _ -> return $ Right ()

resetEngine :: Engine -> IO EngineResult
resetEngine engine = do
    let store = stateStore engine
        logger' = logger engine

    withOperation logger' $ \opCtx -> do
        cleanupResult <- cleanupEngine engine
        case cleanupResult of
            Left err -> do
                logError opCtx "Failed to cleanup engine" $ return ()
                return $ EngineFailure $ StateError $ Text.pack $ show err
            Right _ -> do
                let newState =
                        EngineState
                            { initialized = True
                            , currentConfig = Nothing
                            , currentGeneration = Nothing
                            , operationCount = 0
                            , lastOperation = Nothing
                            }

                logInfo opCtx "Engine reset successfully" $ return ()
                return $ EngineSuccess newState

generateSnapshotId :: IO Text
generateSnapshotId = do
    uuid <- nextRandom
    return $ "engine-snap-" <> Text.pack (UUID.toString uuid)
