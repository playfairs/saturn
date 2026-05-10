{-# LANGUAGE OverloadedStrings #-}

module Mycfg.Errors.Recovery
  ( suggestRecovery
  , canRecover
  , RecoveryAction(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

import Mycfg.Errors.Types

data RecoveryAction
  = FixConfigFile
  | CheckPermissions
  | CleanState
  | ResolveDependencies
  | CheckDiskSpace
  | RetryOperation
  | ManualIntervention
  | CreateBackup
  | CheckPaths
  deriving (Show, Eq)

suggestRecovery :: MycfgError -> [RecoveryAction]
suggestRecovery err = case err of
  ParseError parseErr -> suggestParseRecovery parseErr
  ValidationError validationErr -> suggestValidationRecovery validationErr
  FilesystemError fsErr -> suggestFilesystemRecovery fsErr
  StateError stateErr -> suggestStateRecovery stateErr
  ModuleError moduleErr -> suggestModuleRecovery moduleErr
  EngineError engineErr -> suggestEngineRecovery engineErr

canRecover :: MycfgError -> Bool
canRecover err = case err of
  ParseError _ -> True
  ValidationError _ -> True
  FilesystemError fsErr -> canRecoverFilesystem fsErr
  StateError stateErr -> canRecoverState stateErr
  ModuleError _ -> True
  EngineError engineErr -> canRecoverEngine engineErr

suggestParseRecovery :: ParseError -> [RecoveryAction]
suggestParseRecovery err = case err of
  InvalidToml _ _ -> [FixConfigFile]
  MissingRequiredField _ _ -> [FixConfigFile]
  InvalidFieldType _ _ _ -> [FixConfigFile]
  CircularImports _ -> [FixConfigFile, ManualIntervention]
  InvalidSyntax _ -> [FixConfigFile]

suggestValidationRecovery :: ValidationError -> [RecoveryAction]
suggestValidationRecovery err = case err of
  InvalidPath _ -> [CheckPaths, FixConfigFile]
  PathOutsideHome _ -> [CheckPaths, FixConfigFile]
  ConflictingFiles _ -> [FixConfigFile, ManualIntervention]
  MissingDependency _ -> [ResolveDependencies]
  CyclicDependencies _ -> [FixConfigFile, ManualIntervention]
  InvalidProfileName _ -> [FixConfigFile]
  InvalidGenerationId _ -> [FixConfigFile]

suggestFilesystemRecovery :: FilesystemError -> [RecoveryAction]
suggestFilesystemRecovery err = case err of
  PermissionDenied _ -> [CheckPermissions, ManualIntervention]
  FileNotFound _ -> [CheckPaths, ManualIntervention]
  DirectoryNotFound _ -> [CheckPaths, ManualIntervention]
  SymlinkLoop _ -> [CheckPaths, ManualIntervention]
  AtomicWriteFailed _ -> [CheckPermissions, CheckDiskSpace, RetryOperation]
  InvalidPermissions _ -> [CheckPermissions, ManualIntervention]
  DiskSpaceError -> [CheckDiskSpace, ManualIntervention]
  ReadOnlyFileSystem _ -> [ManualIntervention]

suggestStateRecovery :: StateError -> [RecoveryAction]
suggestStateRecovery err = case err of
  StateDirectoryNotFound -> [CleanState]
  GenerationNotFound _ -> [CleanState, ManualIntervention]
  CorruptedState _ -> [CleanState, ManualIntervention]
  LockAcquisitionFailed -> [RetryOperation, ManualIntervention]
  RollbackFailed _ -> [CleanState, ManualIntervention]
  SnapshotGenerationFailed -> [CheckDiskSpace, CheckPermissions, RetryOperation]

suggestModuleRecovery :: ModuleError -> [RecoveryAction]
suggestModuleRecovery err = case err of
  ModuleNotFound _ -> [FixConfigFile, ResolveDependencies]
  InvalidModuleStructure _ -> [FixConfigFile, ManualIntervention]
  ModuleLoadFailed _ _ -> [FixConfigFile, ManualIntervention]
  DependencyResolutionFailed _ -> [ResolveDependencies, FixConfigFile]
  ModuleExecutionFailed _ _ -> [FixConfigFile, ManualIntervention]

suggestEngineRecovery :: EngineError -> [RecoveryAction]
suggestEngineRecovery err = case err of
  PlanExecutionFailed _ -> [RetryOperation, ManualIntervention]
  RollbackIncomplete -> [CleanState, ManualIntervention]
  DryRunValidationFailed -> [FixConfigFile]
  IdempotencyCheckFailed _ -> [CleanState, ManualIntervention]
  DependencyGraphInvalid -> [FixConfigFile, ResolveDependencies]
  ApplyTimeout -> [RetryOperation, ManualIntervention]

canRecoverFilesystem :: FilesystemError -> Bool
canRecoverFilesystem err = case err of
  PermissionDenied _ -> True
  FileNotFound _ -> True
  DirectoryNotFound _ -> True
  SymlinkLoop _ -> True
  AtomicWriteFailed _ -> True
  InvalidPermissions _ -> True
  DiskSpaceError -> True
  ReadOnlyFileSystem _ -> False

canRecoverState :: StateError -> Bool
canRecoverState err = case err of
  StateDirectoryNotFound -> True
  GenerationNotFound _ -> True
  CorruptedState _ -> True
  LockAcquisitionFailed -> True
  RollbackFailed _ -> True
  SnapshotGenerationFailed -> True

canRecoverEngine :: EngineError -> Bool
canRecoverEngine err = case err of
  PlanExecutionFailed _ -> True
  RollbackIncomplete -> True
  DryRunValidationFailed -> True
  IdempotencyCheckFailed _ -> True
  DependencyGraphInvalid -> True
  ApplyTimeout -> True

recoveryActionDescription :: RecoveryAction -> Text
recoveryActionDescription action = case action of
  FixConfigFile -> "Fix the configuration file syntax and structure"
  CheckPermissions -> "Check file and directory permissions"
  CleanState -> "Clean the state directory and start fresh"
  ResolveDependencies -> "Resolve missing or conflicting dependencies"
  CheckDiskSpace -> "Check available disk space"
  RetryOperation -> "Retry the operation"
  ManualIntervention -> "Manual intervention required"
  CreateBackup -> "Create a backup before proceeding"
  CheckPaths -> "Verify all file paths are correct and accessible"
