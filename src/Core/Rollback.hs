{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mycfg.Core.Rollback (
    RollbackOptions (..),
    RollbackResult (..),
    RollbackContext (..),
    RollbackError (..),
    RollbackStrategy (..),
    createRollbackPlan,
    executeRollback,
    rollbackToGeneration,
    rollbackToSnapshot,
    validateRollback,
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

import Mycfg.Config.Types
import Mycfg.Core.Diff
import Mycfg.Core.Planner
import Mycfg.Errors.Types
import Mycfg.Filesystem.Atomic
import Mycfg.Filesystem.Copy
import Mycfg.Filesystem.Permissions
import Mycfg.Filesystem.Symlink
import Mycfg.State.Generations
import Mycfg.State.Manifest
import Mycfg.State.Snapshot
import Mycfg.State.Store

data RollbackOptions = RollbackOptions
    { force :: Bool
    , dryRun :: Bool
    , backupBeforeRollback :: Bool
    , validateAfterRollback :: Bool
    , maxRetries :: Int
    , strategy :: RollbackStrategy
    }
    deriving (Show, Eq, Generic)

instance ToJSON RollbackOptions
instance FromJSON RollbackOptions

data RollbackResult = RollbackResult
    { success :: Bool
    , rolledBackSteps :: Int
    , failedSteps :: Int
    , duration :: UTCTime
    , errors :: [RollbackError]
    , warnings :: [Text]
    , fromGeneration :: Maybe Text
    , toGeneration :: Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON RollbackResult
instance FromJSON RollbackResult

data RollbackContext = RollbackContext
    { stateStore :: StateStore
    , options :: RollbackOptions
    , startTime :: UTCTime
    , currentGeneration :: Maybe Generation
    , targetGeneration :: Generation
    }

data RollbackError
    = RollbackExecutionFailed Text
    | RollbackValidationFailed Text
    | RollbackBackupFailed Text
    | RollbackPermissionDenied Text
    | RollbackTargetNotFound Text
    | RollbackIncomplete
    | RollbackTimeout
    deriving (Show, Eq, Generic)

instance ToJSON RollbackError
instance FromJSON RollbackError

data RollbackStrategy
    = GenerationRollback
    | SnapshotRollback
    | ManifestRollback
    | IncrementalRollback
    deriving (Show, Eq, Generic)

instance ToJSON RollbackStrategy
instance FromJSON RollbackStrategy

createRollbackPlan :: StateStore -> Text -> RollbackOptions -> IO (Either RollbackError ExecutionPlan)
createRollbackPlan store targetGenId options = do
    currentGen <- getCurrentGeneration store
    targetGen <- loadGeneration store targetGenId

    case (currentGen, targetGen) of
        (Left err, _) -> return $ Left $ RollbackTargetNotFound $ Text.pack $ show err
        (_, Left err) -> return $ Left $ RollbackTargetNotFound $ Text.pack $ show err
        (Just current, Just target) -> do
            let currentManifest = manifest current
                targetManifest = manifest target

            diffResult <- computeManifestDiff currentManifest targetManifest
            planResult <- createExecutionPlan diffResult

            case planResult of
                PlanFailure err -> return $ Left $ RollbackExecutionFailed $ Text.pack $ show err
                PlanSuccess plan -> return $ Right plan

executeRollback :: RollbackContext -> IO RollbackResult
executeRollback context = do
    let store = stateStore context
        options = Mycfg.Core.Rollback.options context
        targetGen = targetGeneration context

    if dryRun options
        then executeRollbackDryRun context
        else executeRollbackWithPlan context

executeRollbackDryRun :: RollbackContext -> IO RollbackResult
executeRollbackDryRun context = do
    let store = stateStore context
        targetGen = targetGeneration context
        targetGenId = generationId $ metadata targetGen

    planResult <- createRollbackPlan store targetGenId (Mycfg.Core.Rollback.options context)
    case planResult of
        Left err -> return $ createRollbackError context [err]
        Right plan -> do
            dryRunResult <- dryRunPlan plan
            case dryRunResult of
                Left err -> return $ createRollbackError context [RollbackExecutionFailed $ Text.pack $ show err]
                Right _ ->
                    return $
                        RollbackResult
                            { success = True
                            , rolledBackSteps = length (planSteps plan)
                            , failedSteps = 0
                            , duration = 0
                            , errors = []
                            , warnings = ["Dry run completed successfully"]
                            , fromGeneration = fmap (generationId . metadata) (currentGeneration context)
                            , toGeneration = generationId $ metadata targetGen
                            }

executeRollbackWithPlan :: RollbackContext -> IO RollbackResult
executeRollbackWithPlan context = do
    let store = stateStore context
        options = Mycfg.Core.Rollback.options context
        targetGen = targetGeneration context
        targetGenId = generationId $ metadata targetGen

    if backupBeforeRollback options
        then do
            backupResult <- createRollbackBackup context
            case backupResult of
                Left err -> return $ createRollbackError context [err]
                Right _ -> executeRollbackWithBackup context
        else executeRollbackWithoutBackup context

executeRollbackWithBackup :: RollbackContext -> IO RollbackResult
executeRollbackWithBackup context = do
    let store = stateStore context
        targetGen = targetGeneration context
        targetGenId = generationId $ metadata targetGen

    planResult <- createRollbackPlan store targetGenId (Mycfg.Core.Rollback.options context)
    case planResult of
        Left err -> return $ createRollbackError context [err]
        Right plan -> do
            executeResult <- executeRollbackSteps context (planSteps plan)
            case executeResult of
                Left err -> do
                    rollbackFailureResult <- handleRollbackFailure context err
                    return rollbackFailureResult
                Right (rolledBack, failed) -> do
                    validateResult <- validateRollbackResult context
                    case validateResult of
                        Left err -> return $ createRollbackError context [err]
                        Right _ ->
                            return $
                                RollbackResult
                                    { success = failed == 0
                                    , rolledBackSteps = rolledBack
                                    , failedSteps = failed
                                    , duration = 0
                                    , errors = []
                                    , warnings = []
                                    , fromGeneration = fmap (generationId . metadata) (currentGeneration context)
                                    , toGeneration = generationId $ metadata targetGen
                                    }

executeRollbackWithoutBackup :: RollbackContext -> IO RollbackResult
executeRollbackWithoutBackup context = do
    let store = stateStore context
        targetGen = targetGeneration context
        targetGenId = generationId $ metadata targetGen

    planResult <- createRollbackPlan store targetGenId (Mycfg.Core.Rollback.options context)
    case planResult of
        Left err -> return $ createRollbackError context [err]
        Right plan -> do
            executeResult <- executeRollbackSteps context (planSteps plan)
            case executeResult of
                Left err -> return $ createRollbackError context [err]
                Right (rolledBack, failed) -> do
                    validateResult <- validateRollbackResult context
                    case validateResult of
                        Left err -> return $ createRollbackError context [err]
                        Right _ ->
                            return $
                                RollbackResult
                                    { success = failed == 0
                                    , rolledBackSteps = rolledBack
                                    , failedSteps = failed
                                    , duration = 0
                                    , errors = []
                                    , warnings = ["Warning: No backup created before rollback"]
                                    , fromGeneration = fmap (generationId . metadata) (currentGeneration context)
                                    , toGeneration = generationId $ metadata targetGen
                                    }

executeRollbackSteps :: RollbackContext -> [PlanStep] -> IO (Either RollbackError (Int, Int))
executeRollbackSteps context steps = do
    let options = Mycfg.Core.Rollback.options context
        maxRetries' = maxRetries options

    results <- mapM (executeRollbackStepWithRetry options) steps
    let (successes, failures) = partitionEithers results
        rolledBack = length successes
        failed = length failures

    if null failures
        then return $ Right (rolledBack, failed)
        else
            if force options
                then return $ Right (rolledBack, failed)
                else return $ Left $ head failures

executeRollbackStepWithRetry :: RollbackOptions -> PlanStep -> IO (Either RollbackError ())
executeRollbackStepWithRetry options step = do
    let maxRetries' = maxRetries options

    results <- mapM (\attempt -> executeRollbackStep step) [1 .. maxRetries']
    case sequence results of
        Right _ -> return $ Right ()
        Left _ -> return $ Left $ RollbackExecutionFailed $ "Step " <> Text.pack (show (stepId step)) <> " failed after " <> Text.pack (show maxRetries') <> " attempts"

executeRollbackStep :: PlanStep -> IO (Either RollbackError ())
executeRollbackStep step = case stepType step of
    CreateDirectory -> executeRollbackCreateDirectory step
    CopyFile -> executeRollbackCopyFile step
    CreateSymlink -> executeRollbackCreateSymlink step
    RemoveFile -> executeRollbackRemoveFile step
    RemoveDirectory -> executeRollbackRemoveDirectory step
    RemoveSymlink -> executeRollbackRemoveSymlink step
    SetPermissions -> executeRollbackSetPermissions step
    ValidateFile -> executeRollbackValidateFile step

executeRollbackCreateDirectory :: PlanStep -> IO (Either RollbackError ())
executeRollbackCreateDirectory step = do
    let targetDir = takeDirectory (toFilePath (targetPath step))
    result <- createDirectoryIfMissing True targetDir
    case result of
        Left _ -> return $ Left $ RollbackPermissionDenied $ Text.pack targetDir
        Right _ -> return $ Right ()

executeRollbackCopyFile :: PlanStep -> IO (Either RollbackError ())
executeRollbackCopyFile step = do
    case sourcePath step of
        Just source -> do
            result <- safeCopyFile source (targetPath step)
            case result of
                Left err -> return $ Left $ RollbackExecutionFailed $ Text.pack $ show err
                Right _ -> return $ Right ()
        Nothing -> return $ Left $ RollbackTargetNotFound $ Text.pack $ "No source path for step " <> Text.pack (show (stepId step))

executeRollbackCreateSymlink :: PlanStep -> IO (Either RollbackError ())
executeRollbackCreateSymlink step = do
    case sourcePath step of
        Just source -> do
            result <- safeCreateSymlink source (targetPath step)
            case result of
                Left err -> return $ Left $ RollbackExecutionFailed $ Text.pack $ show err
                Right _ -> return $ Right ()
        Nothing -> return $ Left $ RollbackTargetNotFound $ Text.pack $ "No source path for step " <> Text.pack (show (stepId step))

executeRollbackRemoveFile :: PlanStep -> IO (Either RollbackError ())
executeRollbackRemoveFile step = do
    result <- try $ removeFile (toFilePath (targetPath step))
    case result of
        Left (_ :: SomeException) -> return $ Left $ RollbackPermissionDenied $ Text.pack $ toFilePath (targetPath step)
        Right _ -> return $ Right ()

executeRollbackRemoveDirectory :: PlanStep -> IO (Either RollbackError ())
executeRollbackRemoveDirectory step = do
    let targetDir = takeDirectory (toFilePath (targetPath step))
    result <- try $ removeDirectoryRecursive targetDir
    case result of
        Left (_ :: SomeException) -> return $ Left $ RollbackPermissionDenied $ Text.pack targetDir
        Right _ -> return $ Right ()

executeRollbackRemoveSymlink :: PlanStep -> IO (Either RollbackError ())
executeRollbackRemoveSymlink step = do
    result <- safeRemoveSymlink (targetPath step)
    case result of
        Left err -> return $ Left $ RollbackExecutionFailed $ Text.pack $ show err
        Right _ -> return $ Right ()

executeRollbackSetPermissions :: PlanStep -> IO (Either RollbackError ())
executeRollbackSetPermissions step = do
    result <- try $ setPermissions (toFilePath (targetPath step)) emptyPermissions
    case result of
        Left (_ :: SomeException) -> return $ Left $ RollbackPermissionDenied $ Text.pack $ toFilePath (targetPath step)
        Right _ -> return $ Right ()

executeRollbackValidateFile :: PlanStep -> IO (Either RollbackError ())
executeRollbackValidateFile step = do
    exists <- doesFileExist (toFilePath (targetPath step))
    if exists
        then return $ Right ()
        else return $ Left $ RollbackValidationFailed $ Text.pack $ toFilePath (targetPath step)

rollbackToGeneration :: StateStore -> Text -> RollbackOptions -> IO RollbackResult
rollbackToGeneration store targetGenId options = do
    startTime <- getCurrentTime
    currentGen <- getCurrentGeneration store
    targetGen <- loadGeneration store targetGenId

    case (currentGen, targetGen) of
        (Left err, _) -> return $ createRollbackErrorFromStore [RollbackTargetNotFound $ Text.pack $ show err] "" targetGenId
        (_, Left err) -> return $ createRollbackErrorFromStore [RollbackTargetNotFound $ Text.pack $ show err] "" targetGenId
        (Just current, Just target) -> do
            let context =
                    RollbackContext
                        { stateStore = store
                        , options = options
                        , startTime = startTime
                        , currentGeneration = Just current
                        , targetGeneration = target
                        }

            executeRollback context

rollbackToSnapshot :: StateStore -> Text -> RollbackOptions -> IO RollbackResult
rollbackToSnapshot store snapshotId options = do
    startTime <- getCurrentTime
    currentGen <- getCurrentGeneration store

    let snapshotPath = snapshotsDirectory store </> Text.unpack snapshotId ++ ".json"

    snapshotResult <- loadSnapshot snapshotPath
    case snapshotResult of
        Left err -> return $ createRollbackErrorFromStore [RollbackTargetNotFound $ Text.pack $ show err] "" ""
        Right snapshot -> do
            restoreResult <- restoreSnapshot snapshot
            case restoreResult of
                Left err -> return $ createRollbackErrorFromStore [RollbackExecutionFailed $ Text.pack $ show err] "" ""
                Right _ ->
                    return $
                        RollbackResult
                            { success = True
                            , rolledBackSteps = 0
                            , failedSteps = 0
                            , duration = 0
                            , errors = []
                            , warnings = ["Rollback from snapshot completed"]
                            , fromGeneration = fmap (generationId . metadata) currentGen
                            , toGeneration = "snapshot-" <> snapshotId
                            }

validateRollback :: StateStore -> Text -> RollbackOptions -> IO (Either RollbackError ())
validateRollback store targetGenId options = do
    currentGen <- getCurrentGeneration store
    targetGen <- loadGeneration store targetGenId

    case (currentGen, targetGen) of
        (Left err, _) -> return $ Left $ RollbackTargetNotFound $ Text.pack $ show err
        (_, Left err) -> return $ Left $ RollbackTargetNotFound $ Text.pack $ show err
        (Just current, Just target) -> do
            let currentManifest = manifest current
                targetManifest = manifest target

            diffResult <- computeManifestDiff currentManifest targetManifest
            case summary diffResult of
                DiffSummary{totalChanges = 0} -> return $ Right ()
                _ -> return $ Right ()

createRollbackBackup :: RollbackContext -> IO (Either RollbackError ())
createRollbackBackup context = do
    let store = stateStore context
        currentGen = currentGeneration context

    case currentGen of
        Just gen -> do
            snapshotId <- generateSnapshotId
            let description = "Backup before rollback - " ++ Text.unpack (generationId $ metadata gen)

            snapshotResult <- createSnapshot snapshotId (Text.pack description) []
            case snapshotResult of
                Left _ -> return $ Left $ RollbackBackupFailed "Failed to create snapshot"
                Right snapshot -> do
                    let snapshotPath = snapshotsDirectory store </> Text.unpack snapshotId ++ ".json"
                    saveResult <- saveSnapshot snapshot snapshotPath
                    case saveResult of
                        Left _ -> return $ Left $ RollbackBackupFailed "Failed to save snapshot"
                        Right _ -> return $ Right ()
        Nothing -> return $ Right ()

handleRollbackFailure :: RollbackContext -> RollbackError -> IO RollbackResult
handleRollbackFailure context err = do
    let store = stateStore context
        options = Mycfg.Core.Rollback.options context
        targetGen = targetGeneration context

    if force options
        then return $ createRollbackError context [err]
        else do
            restoreResult <- restorePreviousGeneration store targetGen
            case restoreResult of
                Left _ -> return $ createRollbackError context [err, RollbackIncomplete]
                Right _ -> return $ createRollbackError context [err]

restorePreviousGeneration :: StateStore -> Generation -> IO (Either RollbackError ())
restorePreviousGeneration store failedGen = do
    currentGen <- getCurrentGeneration store
    case currentGen of
        Just current | generationId (metadata current) == generationId (metadata failedGen) -> do
            rollbackResult <- rollbackToGeneration store (generationId $ metadata failedGen) defaultRollbackOptions
            case success rollbackResult of
                True -> return $ Right ()
                False -> return $ Left $ RollbackIncomplete
        _ -> return $ Right ()

validateRollbackResult :: RollbackContext -> IO (Either RollbackError ())
validateRollbackResult context = do
    let options = Mycfg.Core.Rollback.options context
        targetGen = targetGeneration context

    if validateAfterRollback options
        then do
            let targetManifest = manifest targetGen
            validationResult <- validateManifest targetManifest
            case validationResult of
                Left err -> return $ Left $ RollbackValidationFailed $ Text.pack $ show err
                Right _ -> return $ Right ()
        else return $ Right ()

createRollbackError :: RollbackContext -> [RollbackError] -> RollbackResult
createRollbackError context errors =
    RollbackResult
        { success = False
        , rolledBackSteps = 0
        , failedSteps = length errors
        , duration = 0
        , errors = errors
        , warnings = []
        , fromGeneration = fmap (generationId . metadata) (currentGeneration context)
        , toGeneration = generationId $ metadata (targetGeneration context)
        }

createRollbackErrorFromStore :: [RollbackError] -> Text -> Text -> RollbackResult
createRollbackErrorFromStore errors fromGen toGen =
    RollbackResult
        { success = False
        , rolledBackSteps = 0
        , failedSteps = length errors
        , duration = 0
        , errors = errors
        , warnings = []
        , fromGeneration = if Text.null fromGen then Nothing else Just fromGen
        , toGeneration = toGen
        }

defaultRollbackOptions :: RollbackOptions
defaultRollbackOptions =
    RollbackOptions
        { force = False
        , dryRun = False
        , backupBeforeRollback = True
        , validateAfterRollback = True
        , maxRetries = 3
        , strategy = GenerationRollback
        }

generateSnapshotId :: IO Text
generateSnapshotId = do
    uuid <- nextRandom
    return $ "rollback-snap-" <> Text.pack (UUID.toString uuid)
