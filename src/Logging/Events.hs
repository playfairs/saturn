{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mycfg.Logging.Events (
    LogLevel (..),
    LogEvent (..),
    OperationId,
    ModuleName,
    Path,
    GenerationId,
    Timestamp,
) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)

type OperationId = Text
type ModuleName = Text
type Path = Text
type GenerationId = Text
type Timestamp = UTCTime

data LogLevel
    = Debug
    | Info
    | Warn
    | Error
    deriving (Show, Eq, Ord)

instance ToJSON LogLevel where
    toJSON Debug = "debug"
    toJSON Info = "info"
    toJSON Warn = "warn"
    toJSON Error = "error"

data LogEvent
    = SystemStarted
        { operationId :: OperationId
        , timestamp :: Timestamp
        , version :: Text
        }
    | SystemShutdown
        { operationId :: OperationId
        , timestamp :: Timestamp
        }
    | ConfigLoaded
        { operationId :: OperationId
        , timestamp :: Timestamp
        , configPath :: Path
        , modules :: [ModuleName]
        }
    | ConfigParseFailed
        { operationId :: OperationId
        , timestamp :: Timestamp
        , configPath :: Path
        , error :: Text
        }
    | ValidationStarted
        { operationId :: OperationId
        , timestamp :: Timestamp
        }
    | ValidationCompleted
        { operationId :: OperationId
        , timestamp :: Timestamp
        , warnings :: [Text]
        }
    | ValidationFailed
        { operationId :: OperationId
        , timestamp :: Timestamp
        , errors :: [Text]
        }
    | ModuleLoaded
        { operationId :: OperationId
        , timestamp :: Timestamp
        , moduleName :: ModuleName
        , dependencies :: [ModuleName]
        }
    | ModuleLoadFailed
        { operationId :: OperationId
        , timestamp :: Timestamp
        , moduleName :: ModuleName
        , error :: Text
        }
    | DependencyResolutionStarted
        { operationId :: OperationId
        , timestamp :: Timestamp
        }
    | DependencyResolutionCompleted
        { operationId :: OperationId
        , timestamp :: Timestamp
        , resolvedOrder :: [ModuleName]
        }
    | DependencyResolutionFailed
        { operationId :: OperationId
        , timestamp :: Timestamp
        , conflicts :: [ModuleName]
        }
    | PlanningStarted
        { operationId :: OperationId
        , timestamp :: Timestamp
        }
    | PlanningCompleted
        { operationId :: OperationId
        , timestamp :: Timestamp
        , operations :: Int
        }
    | PlanningFailed
        { operationId :: OperationId
        , timestamp :: Timestamp
        , error :: Text
        }
    | ApplyStarted
        { operationId :: OperationId
        , timestamp :: Timestamp
        , dryRun :: Bool
        }
    | ApplyCompleted
        { operationId :: OperationId
        , timestamp :: Timestamp
        , operationsApplied :: Int
        , dryRun :: Bool
        }
    | ApplyFailed
        { operationId :: OperationId
        , timestamp :: Timestamp
        , error :: Text
        , partialOperations :: Int
        }
    | FileOperation
        { operationId :: OperationId
        , timestamp :: Timestamp
        , operation :: Text
        , sourcePath :: Path
        , targetPath :: Path
        , success :: Bool
        }
    | SymlinkCreated
        { operationId :: OperationId
        , timestamp :: Timestamp
        , sourcePath :: Path
        , targetPath :: Path
        }
    | SymlinkRemoved
        { operationId :: OperationId
        , timestamp :: Timestamp
        , targetPath :: Path
        }
    | FileCopied
        { operationId :: OperationId
        , timestamp :: Timestamp
        , sourcePath :: Path
        , targetPath :: Path
        }
    | FileRemoved
        { operationId :: OperationId
        , timestamp :: Timestamp
        , targetPath :: Path
        }
    | DirectoryCreated
        { operationId :: OperationId
        , timestamp :: Timestamp
        , path :: Path
        }
    | PermissionsChanged
        { operationId :: OperationId
        , timestamp :: Timestamp
        , path :: Path
        , permissions :: Text
        }
    | GenerationCreated
        { operationId :: OperationId
        , timestamp :: Timestamp
        , generationId :: GenerationId
        , description :: Text
        }
    | GenerationActivated
        { operationId :: OperationId
        , timestamp :: Timestamp
        , generationId :: GenerationId
        }
    | GenerationRolledBack
        { operationId :: OperationId
        , timestamp :: Timestamp
        , fromGeneration :: GenerationId
        , toGeneration :: GenerationId
        }
    | SnapshotCreated
        { operationId :: OperationId
        , timestamp :: Timestamp
        , snapshotId :: Text
        , files :: Int
        }
    | StateLocked
        { operationId :: OperationId
        , timestamp :: Timestamp
        , lockFile :: Path
        }
    | StateUnlocked
        { operationId :: OperationId
        , timestamp :: Timestamp
        , lockFile :: Path
        }
    | StateCorruptionDetected
        { operationId :: OperationId
        , timestamp :: Timestamp
        , path :: Path
        , error :: Text
        }
    | RollbackStarted
        { operationId :: OperationId
        , timestamp :: Timestamp
        , generationId :: GenerationId
        }
    | RollbackCompleted
        { operationId :: OperationId
        , timestamp :: Timestamp
        , operationsReverted :: Int
        }
    | RollbackFailed
        { operationId :: OperationId
        , timestamp :: Timestamp
        , error :: Text
        }
    | DriftDetected
        { operationId :: OperationId
        , timestamp :: Timestamp
        , path :: Path
        , expectedState :: Text
        , actualState :: Text
        }
    | IdempotencyCheck
        { operationId :: OperationId
        , timestamp :: Timestamp
        , path :: Path
        , isIdempotent :: Bool
        }
    | CustomEvent
        { operationId :: OperationId
        , timestamp :: Timestamp
        , level :: LogLevel
        , category :: Text
        , message :: Text
        , details :: [(Text, Text)]
        }
    deriving (Show, Eq)

getLogLevel :: LogEvent -> LogLevel
getLogLevel event = case event of
    SystemStarted{} -> Info
    SystemShutdown{} -> Info
    ConfigLoaded{} -> Info
    ConfigParseFailed{} -> Error
    ValidationStarted{} -> Debug
    ValidationCompleted{} -> Info
    ValidationFailed{} -> Error
    ModuleLoaded{} -> Debug
    ModuleLoadFailed{} -> Error
    DependencyResolutionStarted{} -> Debug
    DependencyResolutionCompleted{} -> Debug
    DependencyResolutionFailed{} -> Error
    PlanningStarted{} -> Debug
    PlanningCompleted{} -> Info
    PlanningFailed{} -> Error
    ApplyStarted{} -> Info
    ApplyCompleted{} -> Info
    ApplyFailed{} -> Error
    FileOperation{success = True} -> Debug
    FileOperation{success = False} -> Warn
    SymlinkCreated{} -> Debug
    SymlinkRemoved{} -> Debug
    FileCopied{} -> Debug
    FileRemoved{} -> Debug
    DirectoryCreated{} -> Debug
    PermissionsChanged{} -> Debug
    GenerationCreated{} -> Info
    GenerationActivated{} -> Info
    GenerationRolledBack{} -> Info
    SnapshotCreated{} -> Debug
    StateLocked{} -> Debug
    StateUnlocked{} -> Debug
    StateCorruptionDetected{} -> Error
    RollbackStarted{} -> Info
    RollbackCompleted{} -> Info
    RollbackFailed{} -> Error
    DriftDetected{} -> Warn
    IdempotencyCheck{isIdempotent = True} -> Debug
    IdempotencyCheck{isIdempotent = False} -> Warn
    CustomEvent{level = l} -> l

getOperationId :: LogEvent -> OperationId
getOperationId event = case event of
    SystemStarted{operationId = oid} -> oid
    SystemShutdown{operationId = oid} -> oid
    ConfigLoaded{operationId = oid} -> oid
    ConfigParseFailed{operationId = oid} -> oid
    ValidationStarted{operationId = oid} -> oid
    ValidationCompleted{operationId = oid} -> oid
    ValidationFailed{operationId = oid} -> oid
    ModuleLoaded{operationId = oid} -> oid
    ModuleLoadFailed{operationId = oid} -> oid
    DependencyResolutionStarted{operationId = oid} -> oid
    DependencyResolutionCompleted{operationId = oid} -> oid
    DependencyResolutionFailed{operationId = oid} -> oid
    PlanningStarted{operationId = oid} -> oid
    PlanningCompleted{operationId = oid} -> oid
    PlanningFailed{operationId = oid} -> oid
    ApplyStarted{operationId = oid} -> oid
    ApplyCompleted{operationId = oid} -> oid
    ApplyFailed{operationId = oid} -> oid
    FileOperation{operationId = oid} -> oid
    SymlinkCreated{operationId = oid} -> oid
    SymlinkRemoved{operationId = oid} -> oid
    FileCopied{operationId = oid} -> oid
    FileRemoved{operationId = oid} -> oid
    DirectoryCreated{operationId = oid} -> oid
    PermissionsChanged{operationId = oid} -> oid
    GenerationCreated{operationId = oid} -> oid
    GenerationActivated{operationId = oid} -> oid
    GenerationRolledBack{operationId = oid} -> oid
    SnapshotCreated{operationId = oid} -> oid
    StateLocked{operationId = oid} -> oid
    StateUnlocked{operationId = oid} -> oid
    StateCorruptionDetected{operationId = oid} -> oid
    RollbackStarted{operationId = oid} -> oid
    RollbackCompleted{operationId = oid} -> oid
    RollbackFailed{operationId = oid} -> oid
    DriftDetected{operationId = oid} -> oid
    IdempotencyCheck{operationId = oid} -> oid
    CustomEvent{operationId = oid} -> oid

instance ToJSON LogEvent where
    toJSON event = case event of
        SystemStarted{..} ->
            object
                [ "type" .= ("system_started" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "version" .= version
                ]
        SystemShutdown{..} ->
            object
                [ "type" .= ("system_shutdown" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                ]
        ConfigLoaded{..} ->
            object
                [ "type" .= ("config_loaded" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "config_path" .= configPath
                , "modules" .= modules
                ]
        ConfigParseFailed{..} ->
            object
                [ "type" .= ("config_parse_failed" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "config_path" .= configPath
                , "error" .= error
                ]
        ValidationStarted{..} ->
            object
                [ "type" .= ("validation_started" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                ]
        ValidationCompleted{..} ->
            object
                [ "type" .= ("validation_completed" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "warnings" .= warnings
                ]
        ValidationFailed{..} ->
            object
                [ "type" .= ("validation_failed" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "errors" .= errors
                ]
        ModuleLoaded{..} ->
            object
                [ "type" .= ("module_loaded" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "module_name" .= moduleName
                , "dependencies" .= dependencies
                ]
        ModuleLoadFailed{..} ->
            object
                [ "type" .= ("module_load_failed" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "module_name" .= moduleName
                , "error" .= error
                ]
        DependencyResolutionStarted{..} ->
            object
                [ "type" .= ("dependency_resolution_started" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                ]
        DependencyResolutionCompleted{..} ->
            object
                [ "type" .= ("dependency_resolution_completed" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "resolved_order" .= resolvedOrder
                ]
        DependencyResolutionFailed{..} ->
            object
                [ "type" .= ("dependency_resolution_failed" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "conflicts" .= conflicts
                ]
        PlanningStarted{..} ->
            object
                [ "type" .= ("planning_started" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                ]
        PlanningCompleted{..} ->
            object
                [ "type" .= ("planning_completed" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "operations" .= operations
                ]
        PlanningFailed{..} ->
            object
                [ "type" .= ("planning_failed" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "error" .= error
                ]
        ApplyStarted{..} ->
            object
                [ "type" .= ("apply_started" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "dry_run" .= dryRun
                ]
        ApplyCompleted{..} ->
            object
                [ "type" .= ("apply_completed" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "operations_applied" .= operationsApplied
                , "dry_run" .= dryRun
                ]
        ApplyFailed{..} ->
            object
                [ "type" .= ("apply_failed" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "error" .= error
                , "partial_operations" .= partialOperations
                ]
        FileOperation{..} ->
            object
                [ "type" .= ("file_operation" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "operation" .= operation
                , "source_path" .= sourcePath
                , "target_path" .= targetPath
                , "success" .= success
                ]
        SymlinkCreated{..} ->
            object
                [ "type" .= ("symlink_created" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "source_path" .= sourcePath
                , "target_path" .= targetPath
                ]
        SymlinkRemoved{..} ->
            object
                [ "type" .= ("symlink_removed" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "target_path" .= targetPath
                ]
        FileCopied{..} ->
            object
                [ "type" .= ("file_copied" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "source_path" .= sourcePath
                , "target_path" .= targetPath
                ]
        FileRemoved{..} ->
            object
                [ "type" .= ("file_removed" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "target_path" .= targetPath
                ]
        DirectoryCreated{..} ->
            object
                [ "type" .= ("directory_created" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "path" .= path
                ]
        PermissionsChanged{..} ->
            object
                [ "type" .= ("permissions_changed" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "path" .= path
                , "permissions" .= permissions
                ]
        GenerationCreated{..} ->
            object
                [ "type" .= ("generation_created" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "generation_id" .= generationId
                , "description" .= description
                ]
        GenerationActivated{..} ->
            object
                [ "type" .= ("generation_activated" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "generation_id" .= generationId
                ]
        GenerationRolledBack{..} ->
            object
                [ "type" .= ("generation_rolled_back" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "from_generation" .= fromGeneration
                , "to_generation" .= toGeneration
                ]
        SnapshotCreated{..} ->
            object
                [ "type" .= ("snapshot_created" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "snapshot_id" .= snapshotId
                , "files" .= files
                ]
        StateLocked{..} ->
            object
                [ "type" .= ("state_locked" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "lock_file" .= lockFile
                ]
        StateUnlocked{..} ->
            object
                [ "type" .= ("state_unlocked" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "lock_file" .= lockFile
                ]
        StateCorruptionDetected{..} ->
            object
                [ "type" .= ("state_corruption_detected" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "path" .= path
                , "error" .= error
                ]
        RollbackStarted{..} ->
            object
                [ "type" .= ("rollback_started" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "generation_id" .= generationId
                ]
        RollbackCompleted{..} ->
            object
                [ "type" .= ("rollback_completed" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "operations_reverted" .= operationsReverted
                ]
        RollbackFailed{..} ->
            object
                [ "type" .= ("rollback_failed" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "error" .= error
                ]
        DriftDetected{..} ->
            object
                [ "type" .= ("drift_detected" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "path" .= path
                , "expected_state" .= expectedState
                , "actual_state" .= actualState
                ]
        IdempotencyCheck{..} ->
            object
                [ "type" .= ("idempotency_check" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "path" .= path
                , "is_idempotent" .= isIdempotent
                ]
        CustomEvent{..} ->
            object
                [ "type" .= ("custom" :: Text)
                , "operation_id" .= operationId
                , "timestamp" .= timestamp
                , "level" .= level
                , "category" .= category
                , "message" .= message
                , "details" .= details
                ]
