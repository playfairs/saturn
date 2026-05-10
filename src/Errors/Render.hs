{-# LANGUAGE OverloadedStrings #-}

module Mycfg.Errors.Render
  ( renderError
  , renderErrorCompact
  , renderErrorJson
  ) where

import Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as Text
import Prettyprinter (Doc, Pretty(..), align, brackets, colon, comma, dquotes, encloseSep, hardline, indent, line, parens, pretty, space, vsep)
import Prettyprinter.Render.Terminal (AnsiStyle, Color(..), color, colorDull, bold, italic)

import Mycfg.Errors.Types

renderError :: MycfgError -> Doc AnsiStyle
renderError err = case err of
  ParseError parseErr -> renderParseError parseErr
  ValidationError validationErr -> renderValidationError validationErr
  FilesystemError fsErr -> renderFilesystemError fsErr
  StateError stateErr -> renderStateError stateErr
  ModuleError moduleErr -> renderModuleError moduleErr
  EngineError engineErr -> renderEngineError engineErr

renderErrorCompact :: MycfgError -> Text
renderErrorCompact err = case err of
  ParseError (InvalidToml _ msg) -> "Invalid TOML: " <> msg
  ParseError (MissingRequiredField _ field) -> "Missing required field: " <> field
  ParseError (InvalidFieldType _ field expected) -> "Invalid field type for " <> field <> ": expected " <> expected
  ParseError (CircularImports mods) -> "Circular imports: " <> Text.intercalate " -> " mods
  ParseError (InvalidSyntax msg) -> "Invalid syntax: " <> msg
  
  ValidationError (InvalidPath path) -> "Invalid path: " <> path
  ValidationError (PathOutsideHome path) -> "Path outside home directory: " <> path
  ValidationError (ConflictingFiles files) -> "Conflicting files: " <> Text.intercalate ", " files
  ValidationError (MissingDependency dep) -> "Missing dependency: " <> dep
  ValidationError (CyclicDependencies deps) -> "Cyclic dependencies: " <> Text.intercalate " -> " deps
  ValidationError (InvalidProfileName name) -> "Invalid profile name: " <> name
  ValidationError (InvalidGenerationId genId) -> "Invalid generation ID: " <> genId
  
  FilesystemError (PermissionDenied path) -> "Permission denied: " <> path
  FilesystemError (FileNotFound path) -> "File not found: " <> path
  FilesystemError (DirectoryNotFound path) -> "Directory not found: " <> path
  FilesystemError (SymlinkLoop path) -> "Symlink loop detected: " <> path
  FilesystemError (AtomicWriteFailed path) -> "Atomic write failed: " <> path
  FilesystemError (InvalidPermissions path) -> "Invalid permissions: " <> path
  FilesystemError DiskSpaceError -> "Insufficient disk space"
  FilesystemError (ReadOnlyFileSystem path) -> "Read-only filesystem: " <> path
  
  StateError StateDirectoryNotFound -> "State directory not found"
  StateError (GenerationNotFound genId) -> "Generation not found: " <> genId
  StateError (CorruptedState msg) -> "Corrupted state: " <> msg
  StateError LockAcquisitionFailed -> "Failed to acquire lock"
  StateError (RollbackFailed genId) -> "Rollback failed for generation: " <> genId
  StateError SnapshotGenerationFailed -> "Failed to generate snapshot"
  
  ModuleError (ModuleNotFound name) -> "Module not found: " <> name
  ModuleError (InvalidModuleStructure name) -> "Invalid module structure: " <> name
  ModuleError (ModuleLoadFailed name msg) -> "Module load failed: " <> name <> " - " <> msg
  ModuleError (DependencyResolutionFailed deps) -> "Dependency resolution failed for: " <> Text.intercalate ", " deps
  ModuleError (ModuleExecutionFailed name msg) -> "Module execution failed: " <> name <> " - " <> msg
  
  EngineError (PlanExecutionFailed msg) -> "Plan execution failed: " <> msg
  EngineError RollbackIncomplete -> "Rollback incomplete"
  EngineError DryRunValidationFailed -> "Dry run validation failed"
  EngineError (IdempotencyCheckFailed msg) -> "Idempotency check failed: " <> msg
  EngineError DependencyGraphInvalid -> "Invalid dependency graph"
  EngineError ApplyTimeout -> "Apply operation timed out"

renderErrorJson :: MycfgError -> Text
renderErrorJson err = case err of
  ParseError parseErr -> renderParseErrorJson parseErr
  ValidationError validationErr -> renderValidationErrorJson validationErr
  FilesystemError fsErr -> renderFilesystemErrorJson fsErr
  StateError stateErr -> renderStateErrorJson stateErr
  ModuleError moduleErr -> renderModuleErrorJson moduleErr
  EngineError engineErr -> renderEngineErrorJson engineErr

renderParseError :: ParseError -> Doc AnsiStyle
renderParseError err = case err of
  InvalidToml path msg ->
    color Red (bold "Parse Error:") <+> "Invalid TOML in" <+> pretty (show path) <> line <>
    indent 2 (color Yellow (dquotes (pretty msg)))
  
  MissingRequiredField path field ->
    color Red (bold "Parse Error:") <+> "Missing required field" <+> color Yellow (dquotes (pretty field)) <+> "in" <+> pretty (show path)
  
  InvalidFieldType path field expected ->
    color Red (bold "Parse Error:") <+> "Invalid field type for" <+> color Yellow (dquotes (pretty field)) <+> "in" <+> pretty (show path) <> line <>
    indent 2 ("Expected:" <+> color Green (pretty expected))
  
  CircularImports modules ->
    color Red (bold "Parse Error:") <+> "Circular imports detected" <> line <>
    indent 2 (vsep (map (color Yellow . pretty) modules))
  
  InvalidSyntax msg ->
    color Red (bold "Parse Error:") <+> color Yellow (dquotes (pretty msg))

renderValidationError :: ValidationError -> Doc AnsiStyle
renderValidationError err = case err of
  InvalidPath path ->
    color Red (bold "Validation Error:") <+> "Invalid path:" <+> color Yellow (pretty path)
  
  PathOutsideHome path ->
    color Red (bold "Validation Error:") <+> "Path outside home directory:" <+> color Yellow (pretty path)
  
  ConflictingFiles files ->
    color Red (bold "Validation Error:") <+> "Conflicting files:" <> line <>
    indent 2 (vsep (map (color Yellow . pretty) files))
  
  MissingDependency dep ->
    color Red (bold "Validation Error:") <+> "Missing dependency:" <+> color Yellow (pretty dep)
  
  CyclicDependencies deps ->
    color Red (bold "Validation Error:") <+> "Cyclic dependencies:" <> line <>
    indent 2 (vsep (map (color Yellow . pretty) deps))
  
  InvalidProfileName name ->
    color Red (bold "Validation Error:") <+> "Invalid profile name:" <+> color Yellow (pretty name)
  
  InvalidGenerationId genId ->
    color Red (bold "Validation Error:") <+> "Invalid generation ID:" <+> color Yellow (pretty genId)

renderFilesystemError :: FilesystemError -> Doc AnsiStyle
renderFilesystemError err = case err of
  PermissionDenied path ->
    color Red (bold "Filesystem Error:") <+> "Permission denied:" <+> color Yellow (pretty path)
  
  FileNotFound path ->
    color Red (bold "Filesystem Error:") <+> "File not found:" <+> color Yellow (pretty path)
  
  DirectoryNotFound path ->
    color Red (bold "Filesystem Error:") <+> "Directory not found:" <+> color Yellow (pretty path)
  
  SymlinkLoop path ->
    color Red (bold "Filesystem Error:") <+> "Symlink loop detected:" <+> color Yellow (pretty path)
  
  AtomicWriteFailed path ->
    color Red (bold "Filesystem Error:") <+> "Atomic write failed:" <+> color Yellow (pretty path)
  
  InvalidPermissions path ->
    color Red (bold "Filesystem Error:") <+> "Invalid permissions:" <+> color Yellow (pretty path)
  
  DiskSpaceError ->
    color Red (bold "Filesystem Error:") <+> "Insufficient disk space"
  
  ReadOnlyFileSystem path ->
    color Red (bold "Filesystem Error:") <+> "Read-only filesystem:" <+> color Yellow (pretty path)

renderStateError :: StateError -> Doc AnsiStyle
renderStateError err = case err of
  StateDirectoryNotFound ->
    color Red (bold "State Error:") <+> "State directory not found"
  
  GenerationNotFound genId ->
    color Red (bold "State Error:") <+> "Generation not found:" <+> color Yellow (pretty genId)
  
  CorruptedState msg ->
    color Red (bold "State Error:") <+> "Corrupted state:" <+> color Yellow (pretty msg)
  
  LockAcquisitionFailed ->
    color Red (bold "State Error:") <+> "Failed to acquire lock"
  
  RollbackFailed genId ->
    color Red (bold "State Error:") <+> "Rollback failed for generation:" <+> color Yellow (pretty genId)
  
  SnapshotGenerationFailed ->
    color Red (bold "State Error:") <+> "Failed to generate snapshot"

renderModuleError :: ModuleError -> Doc AnsiStyle
renderModuleError err = case err of
  ModuleNotFound name ->
    color Red (bold "Module Error:") <+> "Module not found:" <+> color Yellow (pretty name)
  
  InvalidModuleStructure name ->
    color Red (bold "Module Error:") <+> "Invalid module structure:" <+> color Yellow (pretty name)
  
  ModuleLoadFailed name msg ->
    color Red (bold "Module Error:") <+> "Module load failed:" <+> color Yellow (pretty name) <+> "-" <+> color Yellow (pretty msg)
  
  DependencyResolutionFailed deps ->
    color Red (bold "Module Error:") <+> "Dependency resolution failed for:" <> line <>
    indent 2 (vsep (map (color Yellow . pretty) deps))
  
  ModuleExecutionFailed name msg ->
    color Red (bold "Module Error:") <+> "Module execution failed:" <+> color Yellow (pretty name) <+> "-" <+> color Yellow (pretty msg)

renderEngineError :: EngineError -> Doc AnsiStyle
renderEngineError err = case err of
  PlanExecutionFailed msg ->
    color Red (bold "Engine Error:") <+> "Plan execution failed:" <+> color Yellow (pretty msg)
  
  RollbackIncomplete ->
    color Red (bold "Engine Error:") <+> "Rollback incomplete"
  
  DryRunValidationFailed ->
    color Red (bold "Engine Error:") <+> "Dry run validation failed"
  
  IdempotencyCheckFailed msg ->
    color Red (bold "Engine Error:") <+> "Idempotency check failed:" <+> color Yellow (pretty msg)
  
  DependencyGraphInvalid ->
    color Red (bold "Engine Error:") <+> "Invalid dependency graph"
  
  ApplyTimeout ->
    color Red (bold "Engine Error:") <+> "Apply operation timed out"

instance ToJSON MycfgError where
  toJSON err = case err of
    ParseError parseErr -> object ["type" .= ("parse" :: Text), "error" .= parseErr]
    ValidationError validationErr -> object ["type" .= ("validation" :: Text), "error" .= validationErr]
    FilesystemError fsErr -> object ["type" .= ("filesystem" :: Text), "error" .= fsErr]
    StateError stateErr -> object ["type" .= ("state" :: Text), "error" .= stateErr]
    ModuleError moduleErr -> object ["type" .= ("module" :: Text), "error" .= moduleErr]
    EngineError engineErr -> object ["type" .= ("engine" :: Text), "error" .= engineErr]

renderParseErrorJson :: ParseError -> Text
renderParseErrorJson err = case err of
  InvalidToml path msg -> "Invalid TOML in " <> Text.pack (show path) <> ": " <> msg
  MissingRequiredField _ field -> "Missing required field: " <> field
  InvalidFieldType _ field expected -> "Invalid field type for " <> field <> ": expected " <> expected
  CircularImports mods -> "Circular imports: " <> Text.intercalate " -> " mods
  InvalidSyntax msg -> "Invalid syntax: " <> msg

renderValidationErrorJson :: ValidationError -> Text
renderValidationErrorJson err = case err of
  InvalidPath path -> "Invalid path: " <> path
  PathOutsideHome path -> "Path outside home directory: " <> path
  ConflictingFiles files -> "Conflicting files: " <> Text.intercalate ", " files
  MissingDependency dep -> "Missing dependency: " <> dep
  CyclicDependencies deps -> "Cyclic dependencies: " <> Text.intercalate " -> " deps
  InvalidProfileName name -> "Invalid profile name: " <> name
  InvalidGenerationId genId -> "Invalid generation ID: " <> genId

renderFilesystemErrorJson :: FilesystemError -> Text
renderFilesystemErrorJson err = case err of
  PermissionDenied path -> "Permission denied: " <> path
  FileNotFound path -> "File not found: " <> path
  DirectoryNotFound path -> "Directory not found: " <> path
  SymlinkLoop path -> "Symlink loop detected: " <> path
  AtomicWriteFailed path -> "Atomic write failed: " <> path
  InvalidPermissions path -> "Invalid permissions: " <> path
  DiskSpaceError -> "Insufficient disk space"
  ReadOnlyFileSystem path -> "Read-only filesystem: " <> path

renderStateErrorJson :: StateError -> Text
renderStateErrorJson err = case err of
  StateDirectoryNotFound -> "State directory not found"
  GenerationNotFound genId -> "Generation not found: " <> genId
  CorruptedState msg -> "Corrupted state: " <> msg
  LockAcquisitionFailed -> "Failed to acquire lock"
  RollbackFailed genId -> "Rollback failed for generation: " <> genId
  SnapshotGenerationFailed -> "Failed to generate snapshot"

renderModuleErrorJson :: ModuleError -> Text
renderModuleErrorJson err = case err of
  ModuleNotFound name -> "Module not found: " <> name
  InvalidModuleStructure name -> "Invalid module structure: " <> name
  ModuleLoadFailed name msg -> "Module load failed: " <> name <> " - " <> msg
  DependencyResolutionFailed deps -> "Dependency resolution failed for: " <> Text.intercalate ", " deps
  ModuleExecutionFailed name msg -> "Module execution failed: " <> name <> " - " <> msg

renderEngineErrorJson :: EngineError -> Text
renderEngineErrorJson err = case err of
  PlanExecutionFailed msg -> "Plan execution failed: " <> msg
  RollbackIncomplete -> "Rollback incomplete"
  DryRunValidationFailed -> "Dry run validation failed"
  IdempotencyCheckFailed msg -> "Idempotency check failed: " <> msg
  DependencyGraphInvalid -> "Invalid dependency graph"
  ApplyTimeout -> "Apply operation timed out"
