{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mycfg.CLI.Commands (
    runCommand,
    CommandResult (..),
    CommandError (..),
    handleInit,
    handleApply,
    handleRollback,
    handleValidate,
    handleList,
    handleDiff,
    handleDoctor,
    handleHelp,
) where

import Control.Exception (SomeException, bracket, try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class
import Data.Aeson (encode)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Time (getCurrentTime)
import Path (Abs, Dir, File, Path, toFilePath, (</>))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

import Mycfg.CLI.Parser
import Mycfg.Config.Defaults
import Mycfg.Config.Parser
import Mycfg.Config.Validator
import Mycfg.Core.Apply
import Mycfg.Core.Engine
import Mycfg.Core.Rollback
import Mycfg.Errors.Render
import Mycfg.Errors.Types
import Mycfg.Logging.Events
import Mycfg.Logging.Logger
import Mycfg.Modules.Loader
import Mycfg.Modules.Registry
import Mycfg.State.Generations
import Mycfg.State.Store

data CommandResult
    = CommandSuccess Text
    | CommandFailure CommandError
    deriving (Show, Eq)

data CommandError
    = ParseError Text
    | ValidationError Text
    | ExecutionError Text
    | StateError Text
    | ModuleError Text
    | FileSystemError Text
    deriving (Show, Eq)

runCommand :: Options -> IO CommandResult
runCommand options = do
    logger <- initLogger

    case command options of
        Init initOpts -> handleInit logger options initOpts
        Apply applyOpts -> handleApply logger options applyOpts
        Rollback rollbackOpts -> handleRollback logger options rollbackOpts
        Validate validateOpts -> handleValidate logger options validateOpts
        List listOpts -> handleList logger options listOpts
        Diff diffOpts -> handleDiff logger options diffOpts
        Doctor doctorOpts -> handleDoctor logger options doctorOpts
        Help -> handleHelp logger options

handleInit :: Logger -> Options -> InitOptions -> IO CommandResult
handleInit logger options initOpts = do
    withOperation logger $ \opCtx -> do
        logInfo opCtx "Initializing mycfg configuration" $ do
            let configPath = Mycfg.CLI.Parser.configPath initOpts
                statePath = Mycfg.CLI.Parser.statePath initOpts
                force = Mycfg.CLI.Parser.force initOpts
                createExample = Mycfg.CLI.Parser.createExample initOpts

            configExists <- doesFileExist (toFilePath configPath)
            stateExists <- doesDirectoryExist (toFilePath statePath)

            if (configExists || stateExists) && not force
                then return $ CommandFailure $ ValidationError "Configuration or state directory already exists. Use --force to override."
                else do
                    createDirectoryIfMissing True (toFilePath (parent configPath))
                    createDirectoryIfMissing True (toFilePath statePath)

                    if createExample
                        then do
                            let exampleConfig = defaultConfig
                            saveResult <- saveConfigFile exampleConfig configPath
                            case saveResult of
                                Left err -> return $ CommandFailure $ FileSystemError $ Text.pack $ show err
                                Right _ -> return $ CommandSuccess "Configuration initialized with example config"
                        else do
                            let emptyConfig' = emptyConfig
                            saveResult <- saveConfigFile emptyConfig' configPath
                            case saveResult of
                                Left err -> return $ CommandFailure $ FileSystemError $ Text.pack $ show err
                                Right _ -> return $ CommandSuccess "Configuration initialized"

saveConfigFile :: Config -> Path Abs File -> IO (Either MycfgError ())
saveConfigFile config configPath = do
    let configContent = encode config
    result <- atomicWriteText configPath (Text.pack $ show configContent)
    case result of
        Left err -> return $ Left err
        Right _ -> return $ Right ()

handleApply :: Logger -> Options -> ApplyOptions -> IO CommandResult
handleApply logger options applyOpts = do
    withOperation logger $ \opCtx -> do
        logInfo opCtx "Applying configuration" $ do
            let engineConfig =
                    EngineConfig
                        { verbose = verbose options
                        , dryRun = dryRun applyOpts
                        , force = force applyOpts
                        , backupEnabled = backup applyOpts
                        , maxRetries = maxRetries applyOpts
                        , stateDirectory = stateDir options
                        }

            engineResult <- runEngine logger engineConfig
            case engineResult of
                EngineFailure err -> return $ CommandFailure $ ExecutionError $ Text.pack $ show err
                EngineSuccess state -> return $ CommandSuccess "Configuration applied successfully"

handleRollback :: Logger -> Options -> RollbackOptions -> IO CommandResult
handleRollback logger options rollbackOpts = do
    withOperation logger $ \opCtx -> do
        logInfo opCtx "Rolling back configuration" $ do
            storeResult <- initializeStateStore (stateDir options)
            case storeResult of
                Left err -> return $ CommandFailure $ StateError $ Text.pack $ show err
                Right store -> do
                    let targetGen = targetGeneration rollbackOpts
                        rollbackOptions' =
                            Mycfg.Core.Rollback.RollbackOptions
                                { force = force rollbackOpts
                                , dryRun = dryRun rollbackOpts
                                , backupBeforeRollback = backupBeforeRollback rollbackOpts
                                , validateAfterRollback = validateAfterRollback rollbackOpts
                                , maxRetries = maxRetries rollbackOpts
                                , strategy = GenerationRollback
                                }

                    case targetGen of
                        Just genId -> do
                            rollbackResult <- rollbackToGeneration store genId rollbackOptions'
                            case success rollbackResult of
                                True -> return $ CommandSuccess $ "Rollback to generation " <> genId <> " completed"
                                False -> return $ CommandFailure $ ExecutionError $ "Rollback failed: " <> Text.pack (show $ errors rollbackResult)
                        Nothing -> do
                            currentGen <- getCurrentGeneration store
                            case currentGen of
                                Nothing -> return $ CommandFailure $ StateError "No current generation to rollback from"
                                Just current -> do
                                    let previousGenId = generationId $ metadata current
                                    rollbackResult <- rollbackToGeneration store previousGenId rollbackOptions'
                                    case success rollbackResult of
                                        True -> return $ CommandSuccess $ "Rollback to previous generation completed"
                                        False -> return $ CommandFailure $ ExecutionError $ "Rollback failed: " <> Text.pack (show $ errors rollbackResult)

handleValidate :: Logger -> Options -> ValidateOptions -> IO CommandResult
handleValidate logger options validateOpts = do
    withOperation logger $ \opCtx -> do
        logInfo opCtx "Validating configuration" $ do
            let configPath' = Mycfg.CLI.Parser.configFile validateOpts `mplus` configFile options
                strict = strictMode validateOpts
                showWarnings' = showWarnings validateOpts

            configPathResult <- case configPath' of
                Just path -> return $ Right path
                Nothing -> do
                    homeDir <- getHomeDirectory
                    case parseAbsFile (homeDir </> ".config" </> "mycfg" </> "config.toml") of
                        Left _ -> return $ Left "Default configuration file not found"
                        Right path -> return $ Right path

            case configPathResult of
                Left err -> return $ CommandFailure $ ParseError $ Text.pack err
                Right configPath -> do
                    parseResult <- parseConfig configPath
                    case parseResult of
                        Left err -> return $ CommandFailure $ ParseError $ Text.pack $ show err
                        Right config -> do
                            validationResult <- validateConfig config
                            case validationResult of
                                ValidationSuccess warnings -> do
                                    let warningText =
                                            if null warnings && showWarnings'
                                                then "Configuration is valid"
                                                else "Configuration is valid with warnings: " <> Text.intercalate ", " (map Text.pack $ map show warnings)
                                    return $ CommandSuccess warningText
                                ValidationFailure errors -> do
                                    let errorText = "Configuration validation failed: " <> Text.intercalate ", " (map Text.pack $ map show errors)
                                    return $ CommandFailure $ ValidationError errorText

handleList :: Logger -> Options -> ListOptions -> IO CommandResult
handleList logger options listOpts = do
    withOperation logger $ \opCtx -> do
        logInfo opCtx "Listing items" $ do
            storeResult <- initializeStateStore (stateDir options)
            case storeResult of
                Left err -> return $ CommandFailure $ StateError $ Text.pack $ show err
                Right store -> do
                    case listType listOpts of
                        ListGenerations -> do
                            genResult <- listGenerations store
                            case genResult of
                                Left err -> return $ CommandFailure $ StateError $ Text.pack $ show err
                                Right generations -> do
                                    let genList = map (generationId . metadata) generations
                                        output =
                                            if showDetails listOpts
                                                then Text.unlines $ map formatGenerationDetail generations
                                                else Text.unlines genList
                                    return $ CommandSuccess output
                        ListModules -> do
                            homeDir <- getHomeDirectory
                            let modulePaths = [homeDir </> ".config" </> "mycfg" </> "modules"]
                            loader <- createModuleLoader modulePaths
                            let allModules = loadedModules loader
                                output =
                                    if showDetails listOpts
                                        then Text.unlines $ map formatModuleDetail $ Map.elems allModules
                                        else Text.unlines $ Map.keys allModules
                            return $ CommandSuccess output
                        ListProfiles -> do
                            configPath' <- getDefaultConfigPath
                            parseResult <- parseConfigFile configPath'
                            case parseResult of
                                Left err -> return $ CommandFailure $ ParseError $ Text.pack $ show err
                                Right config -> do
                                    let profileList = Map.keys $ profiles config
                                        output =
                                            if showDetails listOpts
                                                then Text.unlines $ map formatProfileDetail $ Map.elems $ profiles config
                                                else Text.unlines profileList
                                    return $ CommandSuccess output
                        ListSnapshots -> do
                            let snapshotDir = snapshotsDirectory store
                                snapshotPath = toFilePath snapshotDir
                            exists <- doesDirectoryExist snapshotPath
                            if not exists
                                then return $ CommandSuccess "No snapshots found"
                                else do
                                    entries <- getDirectoryContents snapshotPath
                                    let snapshots = filter (".json" `isSuffixOf`) entries
                                        output =
                                            if showDetails listOpts
                                                then Text.unlines snapshots
                                                else Text.unlines $ map (takeWhile (/= '.')) snapshots
                                    return $ CommandSuccess output

handleDiff :: Logger -> Options -> DiffOptions -> IO CommandResult
handleDiff logger options diffOpts = do
    withOperation logger $ \opCtx -> do
        logInfo opCtx "Computing differences" $ do
            storeResult <- initializeStateStore (stateDir options)
            case storeResult of
                Left err -> return $ CommandFailure $ StateError $ Text.pack $ show err
                Right store -> do
                    fromGen <- case fromGeneration diffOpts of
                        Just genId -> loadGeneration store genId
                        Nothing -> getCurrentGeneration store
                    toGen <- case toGeneration diffOpts of
                        Just genId -> loadGeneration store genId
                        Nothing -> do
                            configPath' <- getDefaultConfigPath
                            parseResult <- parseConfigFile configPath'
                            case parseResult of
                                Left err -> return $ Left err
                                Right config -> do
                                    genResult <- createGeneration store config "Current configuration"
                                    case genResult of
                                        Left err -> return $ Left err
                                        Right gen -> return $ Right gen

                    case (fromGen, toGen) of
                        (Left err, _) -> return $ CommandFailure $ StateError $ Text.pack $ show err
                        (_, Left err) -> return $ CommandFailure $ StateError $ Text.pack $ show err
                        (Right from, Right to) -> do
                            let diffResult = computeManifestDiff (manifest from) (manifest to)
                                output = formatDiffResult diffResult (showChangesOnly diffOpts)
                            return $ CommandSuccess output

handleDoctor :: Logger -> Options -> DoctorOptions -> IO CommandResult
handleDoctor logger options doctorOpts = do
    withOperation logger $ \opCtx -> do
        logInfo opCtx "Running diagnostics" $ do
            let checkAll = Mycfg.CLI.Parser.checkAll doctorOpts
                checkConfig = Mycfg.CLI.Parser.checkConfig doctorOpts
                checkState = Mycfg.CLI.Parser.checkState doctorOpts
                checkModules = Mycfg.CLI.Parser.checkModules doctorOpts
                fixIssues = Mycfg.CLI.Parser.fixIssues doctorOpts

            issues <- []

            if checkAll || checkConfig
                then do
                    configPath' <- getDefaultConfigPath
                    parseResult <- parseConfigFile configPath'
                    case parseResult of
                        Left err -> issues `seq` return $ CommandFailure $ ParseError $ Text.pack $ show err
                        Right config -> do
                            validationResult <- validateConfig config
                            case validationResult of
                                ValidationFailure errors -> do
                                    let configIssues = map (Text.pack . show) errors
                                    return $ CommandFailure $ ValidationError $ "Configuration issues: " <> Text.intercalate ", " configIssues
                                ValidationSuccess warnings -> do
                                    let warningText =
                                            if null warnings
                                                then "No configuration issues found"
                                                else "Configuration warnings: " <> Text.intercalate ", " (map Text.pack $ map show warnings)
                                    return $ CommandSuccess warningText
                else return $ CommandSuccess "Configuration check skipped"

            if checkAll || checkState
                then do
                    storeResult <- initializeStateStore (stateDir options)
                    case storeResult of
                        Left err -> return $ CommandFailure $ StateError $ Text.pack $ show err
                        Right store -> do
                            validateResult <- validateStateStore store
                            case validateResult of
                                Left err -> return $ CommandFailure $ StateError $ Text.pack $ show err
                                Right _ -> return $ CommandSuccess "State store is valid"
                else return $ CommandSuccess "State check skipped"

            if checkAll || checkModules
                then do
                    homeDir <- getHomeDirectory
                    let modulePaths = [homeDir </> ".config" </> "mycfg" </> "modules"]
                    loader <- createModuleLoader modulePaths
                    let allModules = loadedModules loader
                    return $ CommandSuccess $ "Found " <> Text.pack (show $ Map.size allModules) <> " modules"
                else return $ CommandSuccess "Module check skipped"

handleHelp :: Logger -> Options -> IO CommandResult
handleHelp logger options = do
    withOperation logger $ \opCtx -> do
        logInfo opCtx "Showing help" $ do
            let helpText =
                    "mycfg - Declarative Configuration Manager\n\n"
                        <> "Commands:\n"
                        <> "  init        Initialize a new configuration\n"
                        <> "  apply       Apply configuration changes\n"
                        <> "  rollback    Rollback to a previous generation\n"
                        <> "  validate    Validate configuration\n"
                        <> "  list        List configurations, modules, or generations\n"
                        <> "  diff        Show differences between configurations\n"
                        <> "  doctor      Diagnose and fix configuration issues\n"
                        <> "  help        Show this help message\n\n"
                        <> "Use 'mycfg <command> --help' for more information on a specific command."
            return $ CommandSuccess helpText

formatGenerationDetail :: Generation -> Text
formatGenerationDetail gen =
    let meta = metadata gen
     in generationId meta <> " - " <> description meta <> " (" <> Text.pack (show $ created meta) <> ")"

formatModuleDetail :: LoadedModule -> Text
formatModuleDetail module' =
    let info = moduleInfo module'
     in name info <> " v" <> version info <> " - " <> description info

formatProfileDetail :: ProfileConfig -> Text
formatProfileDetail profile =
    name profile <> " - " <> description profile <> " (modules: " <> Text.intercalate ", " (modules profile) <> ")"

formatDiffResult :: DiffResult -> Bool -> Text
formatDiffResult diffResult changesOnly =
    let summary = summary diffResult
        fileDiffs = Map.elems (fileDiffs diffResult)
        output =
            if changesOnly
                then Text.unlines $ map formatFileDiff $ filter (\d -> diffType d /= Unchanged) fileDiffs
                else Text.unlines $ map formatFileDiff fileDiffs
     in "Summary: " <> Text.pack (show summary) <> "\n\n" <> output

formatFileDiff :: FileDiff -> Text
formatFileDiff diff =
    let diffTypeStr = case diffType diff of
            Added -> "ADD"
            Removed -> "REMOVE"
            Modified -> "MODIFY"
            Unchanged -> "UNCHANGED"
     in diffTypeStr <> " " <> targetPath diff

getDefaultConfigPath :: IO (Path Abs File)
getDefaultConfigPath = do
    homeDir <- getHomeDirectory
    case parseAbsFile (homeDir </> ".config" </> "mycfg" </> "config.toml") of
        Left _ -> error "Failed to parse default config path"
        Right path -> return path

mplus :: Maybe a -> Maybe a -> Maybe a
mplus Nothing y = y
mplus (Just x) _ = Just x
