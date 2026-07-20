{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mycfg.Core.Planner (
    ExecutionPlan (..),
    PlanStep (..),
    PlanStepType (..),
    PlanDependencies (..),
    PlanResult (..),
    PlanError (..),
    createExecutionPlan,
    validateExecutionPlan,
    optimizeExecutionPlan,
    executePlan,
    dryRunPlan,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Graph (Graph, buildG, topSort)
import Data.List (groupBy, sort, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path, toFilePath)

import Mycfg.Config.Types
import Mycfg.Core.Diff
import Mycfg.Filesystem.Atomic
import Mycfg.Filesystem.Copy
import Mycfg.Filesystem.Paths
import Mycfg.Filesystem.Permissions
import Mycfg.Filesystem.Symlink

data ExecutionPlan = ExecutionPlan
    { planSteps :: [PlanStep]
    , planDependencies :: PlanDependencies
    , planMetadata :: PlanMetadata
    }
    deriving (Show, Eq, Generic)

instance ToJSON ExecutionPlan
instance FromJSON ExecutionPlan

data PlanStep = PlanStep
    { stepId :: Int
    , stepType :: PlanStepType
    , stepDescription :: Text
    , sourcePath :: Maybe (Path Abs File)
    , targetPath :: Path Abs File
    , operation :: FileOperation
    , dependencies :: [Int]
    , estimatedTime :: Int
    , rollbackAction :: Maybe PlanStep
    }
    deriving (Show, Eq, Generic)

instance ToJSON PlanStep
instance FromJSON PlanStep

data PlanStepType
    = CreateDirectory
    | CopyFile
    | CreateSymlink
    | RemoveFile
    | RemoveDirectory
    | RemoveSymlink
    | SetPermissions
    | ValidateFile
    deriving (Show, Eq, Generic)

instance ToJSON PlanStepType
instance FromJSON PlanStepType

data PlanDependencies = PlanDependencies
    { fileDependencies :: Map Text [Int]
    , directoryDependencies :: Map Text [Int]
    , orderDependencies :: [(Int, Int)]
    }
    deriving (Show, Eq, Generic)

instance ToJSON PlanDependencies
instance FromJSON PlanDependencies

data PlanMetadata = PlanMetadata
    { planId :: Text
    , created :: UTCTime
    , totalSteps :: Int
    , estimatedDuration :: Int
    , requiresRoot :: Bool
    , dryRunSafe :: Bool
    }
    deriving (Show, Eq, Generic)

instance ToJSON PlanMetadata
instance FromJSON PlanMetadata

data PlanResult
    = PlanSuccess ExecutionPlan
    | PlanFailure PlanError
    deriving (Show, Eq)

data PlanError
    = CircularDependency
    | InvalidStep PlanStep
    | MissingDependency Text
    | PermissionRequired
    | DiskSpaceInsufficient
    | PathConflict Text
    | PlanTimeout
    deriving (Show, Eq)

createExecutionPlan :: DiffResult -> IO PlanResult
createExecutionPlan diffResult = do
    now <- getCurrentTime
    let planId = "plan-" <> Text.pack (show =<< hash now)

    steps <- createPlanSteps diffResult
    dependencies <- createPlanDependencies steps

    let metadata =
            PlanMetadata
                { planId = planId
                , created = now
                , totalSteps = length steps
                , estimatedDuration = sum (map estimatedTime steps)
                , requiresRoot = requiresRootAccess steps
                , dryRunSafe = isDryRunSafe steps
                }

    let plan = ExecutionPlan steps dependencies metadata

    case validateExecutionPlan plan of
        Left err -> return $ PlanFailure err
        Right _ -> do
            optimizedPlan <- optimizeExecutionPlan plan
            return $ PlanSuccess optimizedPlan

createPlanSteps :: DiffResult -> IO [PlanStep]
createPlanSteps diffResult = do
    let fileDiffs = Map.elems (fileDiffs diffResult)
        directoryDiffs = Map.elems (directoryDiffs diffResult)
        symlinkDiffs = Map.elems (symlinkDiffs diffResult)

    fileSteps <- mapM createFilePlanStep fileDiffs
    directorySteps <- mapM createDirectoryPlanStep directoryDiffs
    symlinkSteps <- mapM createSymlinkPlanStep symlinkDiffs

    let allSteps = fileSteps ++ directorySteps ++ symlinkSteps
        sortedSteps = sortBy (comparing stepPriority) allSteps

    return $ zipWith (\step idx -> step{stepId = idx}) sortedSteps [0 ..]

createFilePlanStep :: FileDiff -> IO PlanStep
createFilePlanStep fileDiff = do
    let targetPathStr = Text.unpack (targetPath fileDiff)
        sourcePathStr = fmap Text.unpack (sourcePath fileDiff)

    targetPath <- case parseAbsFile targetPathStr of
        Left _ -> error $ "Invalid target path: " ++ targetPathStr
        Right path -> return path

    sourcePath <- case sourcePathStr of
        Just str -> case parseAbsFile str of
            Left _ -> error $ "Invalid source path: " ++ str
            Right path -> return $ Just path
        Nothing -> return Nothing

    let (stepType, description) = case diffType fileDiff of
            Added -> (CopyFile, "Copy file from " <> fromMaybe "unknown" (sourcePath fileDiff) <> " to " <> targetPath fileDiff)
            Removed -> (RemoveFile, "Remove file " <> targetPath fileDiff)
            Modified -> (CopyFile, "Update file " <> targetPath fileDiff)
            Unchanged -> (ValidateFile, "Validate file " <> targetPath fileDiff)

    return $
        PlanStep
            { stepId = 0
            , stepType = stepType
            , stepDescription = description
            , sourcePath = sourcePath
            , targetPath = targetPath
            , operation = operation fileDiff
            , dependencies = []
            , estimatedTime = estimateStepTime stepType
            , rollbackAction = Nothing
            }

createDirectoryPlanStep :: DirectoryDiff -> IO PlanStep
createDirectoryPlanStep dirDiff = do
    let targetPathStr = Text.unpack (targetPath dirDiff)

    targetPath <- case parseAbsDir targetPathStr of
        Left _ -> error $ "Invalid directory path: " ++ targetPathStr
        Right path -> case parseAbsFile (targetPathStr ++ "/.placeholder") of
            Left _ -> error $ "Invalid directory path: " ++ targetPathStr
            Right file -> return file

    let (stepType, description) = case diffType dirDiff of
            Added -> (CreateDirectory, "Create directory " <> targetPath dirDiff)
            Removed -> (RemoveDirectory, "Remove directory " <> targetPath dirDiff)
            Modified -> (SetPermissions, "Set permissions for directory " <> targetPath dirDiff)
            Unchanged -> (ValidateFile, "Validate directory " <> targetPath dirDiff)

    return $
        PlanStep
            { stepId = 0
            , stepType = stepType
            , stepDescription = description
            , sourcePath = Nothing
            , targetPath = targetPath
            , operation = Copy
            , dependencies = []
            , estimatedTime = estimateStepTime stepType
            , rollbackAction = Nothing
            }

createSymlinkPlanStep :: SymlinkDiff -> IO PlanStep
createSymlinkPlanStep symlinkDiff = do
    let targetPathStr = Text.unpack (targetPath symlinkDiff)

    targetPath <- case parseAbsFile targetPathStr of
        Left _ -> error $ "Invalid symlink path: " ++ targetPathStr
        Right path -> return path

    let (stepType, description) = case diffType symlinkDiff of
            Added -> (CreateSymlink, "Create symlink " <> targetPath symlinkDiff)
            Removed -> (RemoveSymlink, "Remove symlink " <> targetPath symlinkDiff)
            Modified -> (CreateSymlink, "Update symlink " <> targetPath symlinkDiff)
            Unchanged -> (ValidateFile, "Validate symlink " <> targetPath symlinkDiff)

    return $
        PlanStep
            { stepId = 0
            , stepType = stepType
            , stepDescription = description
            , sourcePath = Nothing
            , targetPath = targetPath
            , operation = Symlink
            , dependencies = []
            , estimatedTime = estimateStepTime stepType
            , rollbackAction = Nothing
            }

createPlanDependencies :: [PlanStep] -> IO PlanDependencies
createPlanDependencies steps = do
    let fileDeps = Map.fromList $ map (\step -> (Text.pack (toFilePath (targetPath step)), [stepId step])) steps
        dirDeps = Map.fromList $ map (\step -> (Text.pack (takeDirectory (toFilePath (targetPath step))), [stepId step])) steps

        orderDeps = calculateOrderDependencies steps

    return $ PlanDependencies fileDeps dirDeps orderDeps

calculateOrderDependencies :: [PlanStep] -> [(Int, Int)]
calculateOrderDependencies steps =
    let stepMap = Map.fromList $ map (\step -> (stepId step, step)) steps
        dependencies = concatMap getStepDependencies (Map.elems stepMap)
     in dependencies

getStepDependencies :: PlanStep -> [(Int, Int)]
getStepDependencies step =
    let targetDir = takeDirectory (toFilePath (targetPath step))
        dependentSteps =
            filter
                ( \s ->
                    stepId s /= stepId step
                        && ( toFilePath (targetPath s) == targetDir
                                || isPrefixOf (toFilePath (targetPath step)) (toFilePath (targetPath s))
                           )
                )
                (Map.elems stepMap)
     in map (\s -> (stepId s, stepId step)) dependentSteps

stepPriority :: PlanStep -> Int
stepPriority step = case stepType step of
    CreateDirectory -> 1
    RemoveDirectory -> 2
    RemoveFile -> 3
    RemoveSymlink -> 4
    CopyFile -> 5
    CreateSymlink -> 6
    SetPermissions -> 7
    ValidateFile -> 8

estimateStepTime :: PlanStepType -> Int
estimateStepTime stepType = case stepType of
    CreateDirectory -> 1
    CopyFile -> 10
    CreateSymlink -> 2
    RemoveFile -> 3
    RemoveDirectory -> 5
    RemoveSymlink -> 2
    SetPermissions -> 2
    ValidateFile -> 1

requiresRootAccess :: [PlanStep] -> Bool
requiresRootAccess steps = any requiresRootStep steps
  where
    requiresRootStep step =
        let targetPathStr = toFilePath (targetPath step)
         in "/etc/" `isPrefixOf` targetPathStr
                || "/usr/local/" `isPrefixOf` targetPathStr
                || "/opt/" `isPrefixOf` targetPathStr

isDryRunSafe :: [PlanStep] -> Bool
isDryRunSafe steps = all isDryRunSafeStep steps
  where
    isDryRunSafeStep step = case stepType step of
        ValidateFile -> True
        _ -> True

validateExecutionPlan :: ExecutionPlan -> Either PlanError ()
validateExecutionPlan plan = do
    let steps = planSteps plan
        dependencies = planDependencies plan

    validateSteps steps
    validateDependencies steps dependencies
    validateCircularDependencies dependencies

validateSteps :: [PlanStep] -> Either PlanError ()
validateSteps steps =
    let invalidSteps = filter isInvalidStep steps
     in if null invalidSteps
            then Right ()
            else Left $ InvalidStep $ head invalidSteps

isInvalidStep :: PlanStep -> Bool
isInvalidStep step =
    Text.null (stepDescription step)
        || stepId step < 0
        || estimatedTime step < 0

validateDependencies :: [PlanStep] -> PlanDependencies -> Either PlanError ()
validateDependencies steps dependencies =
    let stepIds = Set.fromList $ map stepId steps
        allDeps =
            concat $
                Map.elems (fileDependencies dependencies)
                    ++ Map.elems (directoryDependencies dependencies)
                    ++ map fst (orderDependencies dependencies)
                    ++ map snd (orderDependencies dependencies)
        invalidDeps = Set.difference (Set.fromList allDeps) stepIds
     in if Set.null invalidDeps
            then Right ()
            else Left $ MissingDependency $ Text.pack $ show $ Set.toList invalidDeps

validateCircularDependencies :: PlanDependencies -> Either PlanError ()
validateCircularDependencies dependencies =
    let graph = buildGraph (orderDependencies dependencies)
        sorted = topSort graph
     in if length sorted == length (orderDependencies dependencies)
            then Right ()
            else Left CircularDependency

buildGraph :: [(Int, Int)] -> Graph
buildGraph edges =
    let maxNode = maximum $ 0 : concatMap (\(a, b) -> [a, b]) edges
     in buildG (0, maxNode) edges

optimizeExecutionPlan :: ExecutionPlan -> IO ExecutionPlan
optimizeExecutionPlan plan = do
    let steps = planSteps plan
        optimizedSteps = optimizeSteps steps
        optimizedDependencies = calculateOrderDependencies optimizedSteps

        metadata = planMetadata plan
        optimizedMetadata =
            metadata
                { totalSteps = length optimizedSteps
                , estimatedDuration = sum (map estimatedTime optimizedSteps)
                }

    return $
        plan
            { planSteps = optimizedSteps
            , planDependencies = (planDependencies plan){orderDependencies = optimizedDependencies}
            , planMetadata = optimizedMetadata
            }

optimizeSteps :: [PlanStep] -> [PlanStep]
optimizeSteps steps =
    let grouped = groupBy (\a b -> stepType a == stepType b) $ sortBy (comparing stepType) steps
        optimizedGroups = map optimizeGroup grouped
     in concat optimizedGroups

optimizeGroup :: [PlanStep] -> [PlanStep]
optimizeGroup group =
    let sortedByPath = sortBy (comparing (toFilePath . targetPath)) group
     in sortedByPath

executePlan :: ExecutionPlan -> IO (Either PlanError ())
executePlan plan = do
    let steps = planSteps plan
        sortedSteps = sortStepsByDependencies steps

    results <- mapM executePlanStep sortedSteps
    let errors = [err | Left err <- results]

    if null errors
        then return $ Right ()
        else return $ Left $ head errors

sortStepsByDependencies :: [PlanStep] -> [PlanStep]
sortStepsByDependencies steps =
    let graph = buildGraph $ concatMap (\step -> map (\dep -> (dep, stepId step)) (dependencies step)) steps
        sortedIds = topSort graph
        stepMap = Map.fromList $ map (\step -> (stepId step, step)) steps
     in map (\stepId -> Map.findWithDefault (error "Step not found") stepId stepMap) sortedIds

executePlanStep :: PlanStep -> IO (Either PlanError ())
executePlanStep step = case stepType step of
    CreateDirectory -> executeCreateDirectory step
    CopyFile -> executeCopyFile step
    CreateSymlink -> executeCreateSymlink step
    RemoveFile -> executeRemoveFile step
    RemoveDirectory -> executeRemoveDirectory step
    RemoveSymlink -> executeRemoveSymlink step
    SetPermissions -> executeSetPermissions step
    ValidateFile -> executeValidateFile step

executeCreateDirectory :: PlanStep -> IO (Either PlanError ())
executeCreateDirectory step = do
    let targetDir = takeDirectory (toFilePath (targetPath step))
    result <- createDirectoryIfMissing True targetDir
    case result of
        Left _ -> return $ Left PermissionRequired
        Right _ -> return $ Right ()

executeCopyFile :: PlanStep -> IO (Either PlanError ())
executeCopyFile step = do
    case sourcePath step of
        Just source -> do
            result <- safeCopyFile source (targetPath step)
            case result of
                Left _ -> return $ Left PermissionRequired
                Right _ -> return $ Right ()
        Nothing -> return $ Left $ InvalidStep step

executeCreateSymlink :: PlanStep -> IO (Either PlanError ())
executeCreateSymlink step = do
    case sourcePath step of
        Just source -> do
            result <- safeCreateSymlink source (targetPath step)
            case result of
                Left _ -> return $ Left PermissionRequired
                Right _ -> return $ Right ()
        Nothing -> return $ Left $ InvalidStep step

executeRemoveFile :: PlanStep -> IO (Either PlanError ())
executeRemoveFile step = do
    result <- try $ removeFile (toFilePath (targetPath step))
    case result of
        Left (_ :: SomeException) -> return $ Left PermissionRequired
        Right _ -> return $ Right ()

executeRemoveDirectory :: PlanStep -> IO (Either PlanError ())
executeRemoveDirectory step = do
    let targetDir = takeDirectory (toFilePath (targetPath step))
    result <- try $ removeDirectoryRecursive targetDir
    case result of
        Left (_ :: SomeException) -> return $ Left PermissionRequired
        Right _ -> return $ Right ()

executeRemoveSymlink :: PlanStep -> IO (Either PlanError ())
executeRemoveSymlink step = do
    result <- safeRemoveSymlink (targetPath step)
    case result of
        Left _ -> return $ Left PermissionRequired
        Right _ -> return $ Right ()

executeSetPermissions :: PlanStep -> IO (Either PlanError ())
executeSetPermissions step = do
    result <- try $ setPermissions (toFilePath (targetPath step)) emptyPermissions
    case result of
        Left (_ :: SomeException) -> return $ Left PermissionRequired
        Right _ -> return $ Right ()

executeValidateFile :: PlanStep -> IO (Either PlanError ())
executeValidateFile step = do
    exists <- doesFileExist (toFilePath (targetPath step))
    if exists
        then return $ Right ()
        else return $ Left $ PathConflict $ Text.pack $ toFilePath (targetPath step)

dryRunPlan :: ExecutionPlan -> IO (Either PlanError ())
dryRunPlan plan = do
    let steps = planSteps plan
    mapM_ dryRunStep steps
    return $ Right ()

dryRunStep :: PlanStep -> IO ()
dryRunStep step = do
    putStrLn $
        "[DRY RUN] "
            ++ Text.unpack (stepDescription step)
            ++ " (ID: "
            ++ show (stepId step)
            ++ ")"
