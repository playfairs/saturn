{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mycfg.Errors.Types (
    MycfgError (..),
    ParseError (..),
    ValidationError (..),
    FilesystemError (..),
    StateError (..),
    ModuleError (..),
    EngineError (..),
    ConfigPath,
    ModuleName,
    ProfileName,
    GenerationId,
) where

import Data.Text (Text)
import Path (Abs, File, Path)

type ConfigPath = Path Abs File
type ModuleName = Text
type ProfileName = Text
type GenerationId = Text

data MycfgError
    = ParseError ParseError
    | ValidationError ValidationError
    | FilesystemError FilesystemError
    | StateError StateError
    | ModuleError ModuleError
    | EngineError EngineError
    deriving (Show, Eq)

data ParseError
    = InvalidToml ConfigPath Text
    | MissingRequiredField ConfigPath Text
    | InvalidFieldType ConfigPath Text Text
    | CircularImports [ModuleName]
    | InvalidSyntax Text
    deriving (Show, Eq)

data ValidationError
    = InvalidPath Text
    | PathOutsideHome Text
    | ConflictingFiles [Text]
    | MissingDependency ModuleName
    | CyclicDependencies [ModuleName]
    | InvalidProfileName ProfileName
    | InvalidGenerationId GenerationId
    deriving (Show, Eq)

data FilesystemError
    = PermissionDenied Text
    | FileNotFound Text
    | DirectoryNotFound Text
    | SymlinkLoop Text
    | AtomicWriteFailed Text
    | InvalidPermissions Text
    | DiskSpaceError
    | ReadOnlyFileSystem Text
    deriving (Show, Eq)

data StateError
    = StateDirectoryNotFound
    | GenerationNotFound GenerationId
    | CorruptedState Text
    | LockAcquisitionFailed
    | RollbackFailed GenerationId
    | SnapshotGenerationFailed
    deriving (Show, Eq)

data ModuleError
    = ModuleNotFound ModuleName
    | InvalidModuleStructure ModuleName
    | ModuleLoadFailed ModuleName Text
    | DependencyResolutionFailed [ModuleName]
    | ModuleExecutionFailed ModuleName Text
    deriving (Show, Eq)

data EngineError
    = PlanExecutionFailed Text
    | RollbackIncomplete
    | DryRunValidationFailed
    | IdempotencyCheckFailed Text
    | DependencyGraphInvalid
    | ApplyTimeout
    deriving (Show, Eq)
