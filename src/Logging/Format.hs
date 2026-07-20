{-# LANGUAGE OverloadedStrings #-}

module Mycfg.Logging.Format (
    formatEvent,
    formatEventJson,
    formatEventCompact,
    LogLevel (..),
    LogEvent (..),
) where

import Data.Aeson (encode)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.Time (defaultTimeLocale, formatTime)
import Prettyprinter (Doc, Pretty (..), align, brackets, colon, comma, dquotes, encloseSep, hardline, indent, line, parens, pretty, space, vsep)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), bold, color, colorDull, italic)

import Mycfg.Logging.Events

formatEvent :: LogEvent -> Doc AnsiStyle
formatEvent event =
    let level = getLogLevel event
        levelDoc = formatLogLevel level
        timeDoc = pretty (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (timestamp event))
        opIdDoc = "[" <> pretty (getOperationId event) <> "]"
     in levelDoc <> space <> timeDoc <> space <> opIdDoc <> colon <> space <> formatEventBody event

formatEventJson :: LogEvent -> Text
formatEventJson event = LazyText.toStrict (encode event)

formatEventCompact :: LogEvent -> Text
formatEventCompact event = case event of
    SystemStarted{..} -> "System started v" <> version
    SystemShutdown{} -> "System shutdown"
    ConfigLoaded{..} -> "Config loaded: " <> configPath
    ConfigParseFailed{..} -> "Config parse failed: " <> error
    ValidationStarted{} -> "Validation started"
    ValidationCompleted{..} -> "Validation completed with " <> Text.pack (show (length warnings)) <> " warnings"
    ValidationFailed{..} -> "Validation failed: " <> Text.intercalate ", " errors
    ModuleLoaded{..} -> "Module loaded: " <> moduleName
    ModuleLoadFailed{..} -> "Module load failed: " <> moduleName <> " - " <> error
    DependencyResolutionStarted{} -> "Dependency resolution started"
    DependencyResolutionCompleted{..} -> "Dependency resolved: " <> Text.intercalate " -> " resolvedOrder
    DependencyResolutionFailed{..} -> "Dependency resolution failed: " <> Text.intercalate ", " conflicts
    PlanningStarted{} -> "Planning started"
    PlanningCompleted{..} -> "Planning completed: " <> Text.pack (show operations) <> " operations"
    PlanningFailed{..} -> "Planning failed: " <> error
    ApplyStarted{dryRun = True} -> "Dry run started"
    ApplyStarted{dryRun = False} -> "Apply started"
    ApplyCompleted{dryRun = True, operationsApplied = ops} -> "Dry run completed: " <> Text.pack (show ops) <> " operations would be applied"
    ApplyCompleted{dryRun = False, operationsApplied = ops} -> "Apply completed: " <> Text.pack (show ops) <> " operations applied"
    ApplyFailed{..} -> "Apply failed: " <> error <> " (" <> Text.pack (show partialOperations) <> " partial)"
    FileOperation{operation = op, sourcePath = src, targetPath = tgt, success = True} -> op <> ": " <> src <> " -> " <> tgt
    FileOperation{operation = op, sourcePath = src, targetPath = tgt, success = False} -> op <> " FAILED: " <> src <> " -> " <> tgt
    SymlinkCreated{..} -> "Symlink created: " <> sourcePath <> " -> " <> targetPath
    SymlinkRemoved{..} -> "Symlink removed: " <> targetPath
    FileCopied{..} -> "File copied: " <> sourcePath <> " -> " <> targetPath
    FileRemoved{..} -> "File removed: " <> targetPath
    DirectoryCreated{..} -> "Directory created: " <> path
    PermissionsChanged{..} -> "Permissions changed: " <> path <> " (" <> permissions <> ")"
    GenerationCreated{..} -> "Generation created: " <> generationId <> " - " <> description
    GenerationActivated{..} -> "Generation activated: " <> generationId
    GenerationRolledBack{..} -> "Generation rolled back: " <> fromGeneration <> " -> " <> toGeneration
    SnapshotCreated{..} -> "Snapshot created: " <> snapshotId <> " (" <> Text.pack (show files) <> " files)"
    StateLocked{..} -> "State locked: " <> lockFile
    StateUnlocked{..} -> "State unlocked: " <> lockFile
    StateCorruptionDetected{..} -> "State corruption detected: " <> path <> " - " <> error
    RollbackStarted{..} -> "Rollback started: " <> generationId
    RollbackCompleted{..} -> "Rollback completed: " <> Text.pack (show operationsReverted) <> " operations reverted"
    RollbackFailed{..} -> "Rollback failed: " <> error
    DriftDetected{..} -> "Drift detected: " <> path <> " (expected: " <> expectedState <> ", actual: " <> actualState <> ")"
    IdempotencyCheck{..} -> "Idempotency check: " <> path <> " (" <> if isIdempotent then "OK" else "FAILED" <> ")"
    CustomEvent{..} -> category <> ": " <> message

formatLogLevel :: LogLevel -> Doc AnsiStyle
formatLogLevel level = case level of
    Debug -> colorDull Blue (bold "DEBUG")
    Info -> color Green (bold "INFO ")
    Warn -> color Yellow (bold "WARN ")
    Error -> color Red (bold "ERROR")

formatEventBody :: LogEvent -> Doc AnsiStyle
formatEventBody event = case event of
    SystemStarted{..} ->
        "System started" <+> "v" <> pretty version
    SystemShutdown{} ->
        "System shutdown"
    ConfigLoaded{..} ->
        "Config loaded:"
            <+> pretty configPath
            <+> if null modules then "" else parens ("modules:" <+> hsep (map pretty modules))
    ConfigParseFailed{..} ->
        "Config parse failed:" <+> pretty configPath
            <> line
            <> indent 2 (color Red (dquotes (pretty error)))
    ValidationStarted{} ->
        "Validation started"
    ValidationCompleted{..} ->
        "Validation completed"
            <+> if null warnings
                then "successfully"
                else parens (color Yellow (pretty (Text.pack (show (length warnings)) <> " warnings")))
    ValidationFailed{..} ->
        "Validation failed:"
            <> line
            <> indent 2 (vsep (map (color Red . pretty) errors))
    ModuleLoaded{..} ->
        "Module loaded:"
            <+> pretty moduleName
            <+> if null dependencies
                then ""
                else parens ("deps:" <+> hsep (map pretty dependencies))
    ModuleLoadFailed{..} ->
        "Module load failed:" <+> pretty moduleName
            <> line
            <> indent 2 (color Red (dquotes (pretty error)))
    DependencyResolutionStarted{} ->
        "Dependency resolution started"
    DependencyResolutionCompleted{..} ->
        "Dependency resolved:" <+> hsep (map pretty resolvedOrder)
    DependencyResolutionFailed{..} ->
        "Dependency resolution failed:" <+> hsep (map pretty conflicts)
    PlanningStarted{} ->
        "Planning started"
    PlanningCompleted{..} ->
        "Planning completed:" <+> pretty operations <+> "operations"
    PlanningFailed{..} ->
        "Planning failed:" <+> color Red (dquotes (pretty error))
    ApplyStarted{dryRun = True} ->
        "Dry run started"
    ApplyStarted{dryRun = False} ->
        "Apply started"
    ApplyCompleted{dryRun = True, operationsApplied = ops} ->
        "Dry run completed:" <+> pretty ops <+> "operations would be applied"
    ApplyCompleted{dryRun = False, operationsApplied = ops} ->
        "Apply completed:" <+> pretty ops <+> "operations applied"
    ApplyFailed{..} ->
        "Apply failed:"
            <+> color Red (dquotes (pretty error))
            <+> parens (color Yellow (pretty (Text.pack (show partialOperations) <> " partial")))
    FileOperation{operation = op, sourcePath = src, targetPath = tgt, success = True} ->
        pretty op <> ":" <+> pretty src <+> "->" <+> pretty tgt
    FileOperation{operation = op, sourcePath = src, targetPath = tgt, success = False} ->
        pretty op <> color Red " FAILED:" <+> pretty src <+> "->" <+> pretty tgt
    SymlinkCreated{..} ->
        "Symlink created:" <+> pretty sourcePath <+> "->" <+> pretty targetPath
    SymlinkRemoved{..} ->
        "Symlink removed:" <+> pretty targetPath
    FileCopied{..} ->
        "File copied:" <+> pretty sourcePath <+> "->" <+> pretty targetPath
    FileRemoved{..} ->
        "File removed:" <+> pretty targetPath
    DirectoryCreated{..} ->
        "Directory created:" <+> pretty path
    PermissionsChanged{..} ->
        "Permissions changed:" <+> pretty path <+> parens (pretty permissions)
    GenerationCreated{..} ->
        "Generation created:" <+> pretty generationId <+> "-" <+> pretty description
    GenerationActivated{..} ->
        "Generation activated:" <+> pretty generationId
    GenerationRolledBack{..} ->
        "Generation rolled back:" <+> pretty fromGeneration <+> "->" <+> pretty toGeneration
    SnapshotCreated{..} ->
        "Snapshot created:" <+> pretty snapshotId <+> parens (pretty files <+> "files")
    StateLocked{..} ->
        "State locked:" <+> pretty lockFile
    StateUnlocked{..} ->
        "State unlocked:" <+> pretty lockFile
    StateCorruptionDetected{..} ->
        "State corruption detected:" <+> pretty path <+> "-" <+> color Red (pretty error)
    RollbackStarted{..} ->
        "Rollback started:" <+> pretty generationId
    RollbackCompleted{..} ->
        "Rollback completed:" <+> pretty operationsReverted <+> "operations reverted"
    RollbackFailed{..} ->
        "Rollback failed:" <+> color Red (dquotes (pretty error))
    DriftDetected{..} ->
        "Drift detected:"
            <+> pretty path
            <+> parens ("expected:" <+> pretty expectedState <> "," <+> "actual:" <+> pretty actualState)
    IdempotencyCheck{..} ->
        "Idempotency check:"
            <+> pretty path
            <+> parens (if isIdempotent then color Green "OK" else color Red "FAILED")
    CustomEvent{..} ->
        pretty category
            <> ":" <+> pretty message
            <> if null details
                then ""
                else line <> indent 2 (vsep (map (\(k, v) -> pretty k <> ":" <+> pretty v) details))
