{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mycfg.Core.Apply (
    ApplyOptions (..),
    ApplyResult (..),
    ApplyContext (..),
    ApplyError (..),
    applyConfiguration,
    applyWithPlan,
    applyDryRun,
    rollbackApply,
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

data ApplyOptions = ApplyOptions
    { dryRun :: Bool
    , force :: Bool
    , backupEnabled :: Bool
    , validateBeforeApply :: Bool
    , continueOnError :: Bool
    , maxRetries :: Int
    }
    deriving (Show, Eq, Generic)

instance ToJSON ApplyOptions
instance FromJSON ApplyOptions

data ApplyResult = ApplyResult
    { success :: Bool
    , appliedSteps :: Int
    , failedSteps :: Int
    , skippedSteps :: Int
    , duration :: UTCTime
    , errors :: [ApplyError]
    , warnings :: [Text]
    , generationId :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON ApplyResult
instance FromJSON ApplyResult

data ApplyContext = ApplyContext
    { stateStore :: StateStore
    , config :: Config
    , options :: ApplyOptions
    , startTime :: UTCTime
    , currentGeneration :: Maybe Generation
    }

data ApplyError
    = StepExecutionFailed Int Text
    | ValidationFailed Text
    | BackupFailed Text
    | RollbackFailed Text
    | PermissionDenied Text
    | DiskSpaceError
    | TimeoutError
    deriving (Show, Eq, Generic)

instance ToJSON ApplyError
instance FromJSON ApplyError

applyConfiguration :: StateStore -> Config -> ApplyOptions -> IO ApplyResult
applyConfiguration store config options = do
    startTime <- getCurrentTime
    currentGen <- getCurrentGeneration store

    let context = ApplyContext store config options startTime currentGen

    if dryRun options
        then applyDryRun context
        else applyWithPlan context

applyWithPlan :: ApplyContext -> IO ApplyResult
applyWithPlan context = do
    let store = stateStore context
        config = Mycfg.Core.Apply.config context
        options = Mycfg.Core.Apply.options context

    currentManifest <- case currentGeneration context of
        Just gen -> return $ Just $ manifest gen
        Nothing -> return Nothing

    diffResult <- computeDiff config currentManifest
    case diffResult of
        Left err -> return $ createErrorResult context [ValidationFailed $ Text.pack $ show err]
        Right diff -> do
            planResult <- createExecutionPlan diff
            case planResult of
                PlanFailure err -> return $ createErrorResult context [ValidationFailed $ Text.pack $ show err]
                PlanSuccess plan -> do
                    if validateBeforeApply options
                        then do
                            validationResult <- validatePlan plan
                            case validationResult of
                                Left err -> return $ createErrorResult context [ValidationFailed $ Text.pack $ show err]
                                Right _ -> executePlanWithContext context plan
                        else executePlanWithContext context plan

applyDryRun :: ApplyContext -> IO ApplyResult
applyDryRun context = do
    let store = stateStore context
        config = Mycfg.Core.Apply.config context

    currentManifest <- case currentGeneration context of
        Just gen -> return $ Just $ manifest gen
        Nothing -> return Nothing

    diffResult <- computeDiff config currentManifest
    case diffResult of
        Left err -> return $ createErrorResult context [ValidationFailed $ Text.pack $ show err]
        Right diff -> do
            planResult <- createExecutionPlan diff
            case planResult of
                PlanFailure err -> return $ createErrorResult context [ValidationFailed $ Text.pack $ show err]
                PlanSuccess plan -> do
                    dryRunResult <- dryRunPlan plan
                    case dryRunResult of
                        Left err -> return $ createErrorResult context [ValidationFailed $ Text.pack $ show err]
                        Right _ ->
                            return $
                                ApplyResult
                                    { success = True
                                    , appliedSteps = 0
                                    , failedSteps = 0
                                    , skippedSteps = 0
                                    , duration = 0
                                    , errors = []
                                    , warnings = ["Dry run completed successfully"]
                                    , generationId = Nothing
                                    }

executePlanWithContext :: ApplyContext -> ExecutionPlan -> IO ApplyResult
executePlanWithContext context plan = do
    let store = stateStore context
        config = Mycfg.Core.Apply.config context
        options = Mycfg.Core.Apply.options context

    if backupEnabled options
        then do
            backupResult <- createBackup context
            case backupResult of
                Left err -> return $ createErrorResult context [BackupFailed $ Text.pack $ show err]
                Right _ -> executePlanWithBackup context plan
        else executePlanWithoutBackup context plan

executePlanWithBackup :: ApplyContext -> ExecutionPlan -> IO ApplyResult
executePlanWithBackup context plan = do
    let store = stateStore context
        config = Mycfg.Core.Apply.config context

    genResult <- createGeneration store config "Applied configuration"
    case genResult of
        Left err -> return $ createErrorResult context [ValidationFailed $ Text.pack $ show err]
        Right generation -> do
            applyResult <- executePlanSteps context (planSteps plan)
            case applyResult of
                Left err -> do
                    rollbackResult <- rollbackToGeneration store (generationId $ metadata generation)
                    case rollbackResult of
                        Left _ -> return $ createErrorResult context [RollbackFailed "Rollback failed after apply failure"]
                        Right _ -> return $ createErrorResult context [err]
                Right (applied, failed, skipped) -> do
                    activateResult <- activateGeneration store (generationId $ metadata generation)
                    case activateResult of
                        Left err -> return $ createErrorResult context [ValidationFailed $ Text.pack $ show err]
                        Right _ ->
                            return $
                                ApplyResult
                                    { success = failed == 0
                                    , appliedSteps = applied
                                    , failedSteps = failed
                                    , skippedSteps = skipped
                                    , duration = 0
                                    , errors = []
                                    , warnings = []
                                    , generationId = Just $ generationId $ metadata generation
                                    }

executePlanWithoutBackup :: ApplyContext -> ExecutionPlan -> IO ApplyResult
executePlanWithoutBackup context plan = do
    applyResult <- executePlanSteps context (planSteps plan)
    case applyResult of
        Left err -> return $ createErrorResult context [err]
        Right (applied, failed, skipped) ->
            return $
                ApplyResult
                    { success = failed == 0
                    , appliedSteps = applied
                    , failedSteps = failed
                    , skippedSteps = skipped
                    , duration = 0
                    , errors = []
                    , warnings = ["Warning: No backup created"]
                    , generationId = Nothing
                    }

executePlanSteps :: ApplyContext -> [PlanStep] -> IO (Either ApplyError (Int, Int, Int))
executePlanSteps context steps = do
    let options = Mycfg.Core.Apply.options context

    results <- mapM (executeStepWithRetry options) steps
    let (successes, failures) = partitionEithers results
        applied = length successes
        failed = length failures
        skipped = 0

    if null failures
        then return $ Right (applied, failed, skipped)
        else
            if continueOnError options
                then return $ Right (applied, failed, skipped)
                else return $ Left $ head failures

executeStepWithRetry :: ApplyOptions -> PlanStep -> IO (Either ApplyError ())
executeStepWithRetry options step = do
    let maxRetries' = maxRetries options

    results <- mapM (\attempt -> executePlanStep step) [1 .. maxRetries']
    case sequence results of
        Right _ -> return $ Right ()
        Left _ -> return $ Left $ StepExecutionFailed (stepId step) $ Text.pack $ "Failed after " ++ show maxRetries' ++ " attempts"

createBackup :: ApplyContext -> IO (Either ApplyError ())
createBackup context = do
    let store = stateStore context

    currentGen <- getCurrentGeneration store
    case currentGen of
        Just gen -> do
            snapshotId <- generateSnapshotId
            let description = "Backup before apply - " ++ Text.unpack (generationId $ metadata gen)

            snapshotResult <- createSnapshot snapshotId (Text.pack description) []
            case snapshotResult of
                Left _ -> return $ Left $ BackupFailed "Failed to create snapshot"
                Right snapshot -> do
                    let snapshotPath = snapshotsDirectory store </> Text.unpack snapshotId ++ ".json"
                    saveResult <- saveSnapshot snapshot snapshotPath
                    case saveResult of
                        Left _ -> return $ Left $ BackupFailed "Failed to save snapshot"
                        Right _ -> return $ Right ()
        Nothing -> return $ Right ()

validatePlan :: ExecutionPlan -> IO (Either ApplyError ())
validatePlan plan = do
    let steps = planSteps plan
        invalidSteps = filter isInvalidStep steps

    if null invalidSteps
        then return $ Right ()
        else return $ Left $ ValidationFailed $ "Invalid steps found: " ++ Text.pack (show invalidSteps)

isInvalidStep :: PlanStep -> Bool
isInvalidStep step =
    Text.null (stepDescription step)
        || stepId step < 0
        || estimatedTime step < 0

createErrorResult :: ApplyContext -> [ApplyError] -> ApplyResult
createErrorResult context errors =
    ApplyResult
        { success = False
        , appliedSteps = 0
        , failedSteps = length errors
        , skippedSteps = 0
        , duration = 0
        , errors = errors
        , warnings = []
        , generationId = Nothing
        }

rollbackApply :: StateStore -> Text -> IO ApplyResult
rollbackApply store generationId = do
    rollbackResult <- rollbackToGeneration store generationId
    case rollbackResult of
        Left err ->
            return $
                ApplyResult
                    { success = False
                    , appliedSteps = 0
                    , failedSteps = 1
                    , skippedSteps = 0
                    , duration = 0
                    , errors = [RollbackFailed $ Text.pack $ show err]
                    , warnings = []
                    , generationId = Just generationId
                    }
        Right _ ->
            return $
                ApplyResult
                    { success = True
                    , appliedSteps = 0
                    , failedSteps = 0
                    , skippedSteps = 0
                    , duration = 0
                    , errors = []
                    , warnings = ["Rollback completed successfully"]
                    , generationId = Just generationId
                    }

generateSnapshotId :: IO Text
generateSnapshotId = do
    uuid <- nextRandom
    return $ "snap-" <> Text.pack (UUID.toString uuid)

generateGenerationId :: IO Text
generateGenerationId = do
    uuid <- nextRandom
    return $ "gen-" <> Text.pack (UUID.toString uuid)

checkDiskSpace :: StateStore -> IO (Either ApplyError ())
checkDiskSpace store = do
    let stateDir = toFilePath $ stateDirectory store

    result <- try $ getDiskSpace stateDir
    case result of
        Left (_ :: SomeException) -> return $ Left DiskSpaceError
        Right (total, free) ->
            if free < 1024 * 1024 * 100 -- 100MB minimum
                then return $ Left DiskSpaceError
                else return $ Right ()

checkPermissions :: StateStore -> IO (Either ApplyError ())
checkPermissions store = do
    let stateDir = toFilePath $ stateDirectory store

    writable <- isDirectoryWritable (stateDirectory store)
    if writable
        then return $ Right ()
        else return $ Left $ PermissionDenied $ Text.pack stateDir

validateApplyContext :: ApplyContext -> IO (Either ApplyError ())
validateApplyContext context = do
    let store = stateStore context
        options = Mycfg.Core.Apply.options context

    spaceResult <- checkDiskSpace store
    case spaceResult of
        Left err -> return $ Left err
        Right _ -> do
            permResult <- checkPermissions store
            case permResult of
                Left err -> return $ Left err
                Right _ -> return $ Right ()

getDiskSpace :: FilePath -> IO (Integer, Integer)
getDiskSpace path = do
    result <- try $ getDiskFreeSpace path
    case result of
        Left (_ :: SomeException) -> return (0, 0)
        Right space -> return space

getDiskFreeSpace :: FilePath -> IO (Integer, Integer)
getDiskFreeSpace path = do
    stat <- getFileSystemStats path
    return (totalSpace stat, freeSpace stat)

data FileSystemStats = FileSystemStats
    { totalSpace :: Integer
    , freeSpace :: Integer
    }
    deriving (Show, Eq)

getFileSystemStats :: FilePath -> IO FileSystemStats
getFileSystemStats path = do
    result <- try $ do
        (total, free) <- getDiskUsage path
        return $ FileSystemStats total free
    case result of
        Left (_ :: SomeException) -> return $ FileSystemStats 0 0
        Right stats -> return stats

getDiskUsage :: FilePath -> IO (Integer, Integer)
getDiskUsage path = do
    result <- try $ readProcess "df" ["-k", path] ""
    case result of
        Left (_ :: SomeException) -> return (0, 0)
        Right output -> parseDfOutput output

parseDfOutput :: String -> IO (Integer, Integer)
parseDfOutput output = do
    let lines' = lines output
    case lines' of
        (_ : dataLine : _) -> do
            let fields = words dataLine
            case fields of
                [_, total, used, free, _, _] -> do
                    let totalK = read total :: Integer
                        freeK = read free :: Integer
                    return (totalK * 1024, freeK * 1024)
                _ -> return (0, 0)
        _ -> return (0, 0)
