{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mycfg.CLI.Parser
  ( Options(..)
  , Command(..)
  , ApplyOptions(..)
  , RollbackOptions(..)
  , ValidateOptions(..)
  , InitOptions(..)
  , ListOptions(..)
  , DiffOptions(..)
  , DoctorOptions(..)
  , parseOptions
  ) where

import Options.Applicative
import Data.Text (Text)
import qualified Data.Text as Text
import Path (Abs, File, Dir)
import System.Exit (exitSuccess)

import Mycfg.Config.Types
import Mycfg.Core.Apply
import Mycfg.Core.Rollback

data Options = Options
  { command :: Command
  , verbose :: Bool
  , quiet :: Bool
  , jsonOutput :: Bool
  , configFile :: Maybe (Path Abs File)
  , stateDir :: Maybe (Path Abs Dir)
  , logLevel :: LogLevel
  } deriving (Show, Eq)

data Command
  = Init InitOptions
  | Apply ApplyOptions
  | Rollback RollbackOptions
  | Validate ValidateOptions
  | List ListOptions
  | Diff DiffOptions
  | Doctor DoctorOptions
  | Help
  deriving (Show, Eq)

data ApplyOptions = ApplyOptions
  { dryRun :: Bool
  , force :: Bool
  , backup :: Bool
  , validate :: Bool
  , continueOnError :: Bool
  , maxRetries :: Int
  } deriving (Show, Eq)

data RollbackOptions = RollbackOptions
  { targetGeneration :: Maybe Text
  , targetSnapshot :: Maybe Text
  , force :: Bool
  , dryRun :: Bool
  , backupBeforeRollback :: Bool
  , validateAfterRollback :: Bool
  , maxRetries :: Int
  } deriving (Show, Eq)

data ValidateOptions = ValidateOptions
  { configFile :: Maybe (Path Abs File)
  , strictMode :: Bool
  , showWarnings :: Bool
  } deriving (Show, Eq)

data InitOptions = InitOptions
  { configPath :: Path Abs File
  , statePath :: Path Abs Dir
  , createExample :: Bool
  , force :: Bool
  } deriving (Show, Eq)

data ListOptions = ListOptions
  { listType :: ListType
  , showDetails :: Bool
  , outputFormat :: OutputFormat
  } deriving (Show, Eq)

data ListType
  = ListGenerations
  | ListModules
  | ListProfiles
  | ListSnapshots
  deriving (Show, Eq)

data OutputFormat
  = TableFormat
  | JsonFormat
  | YamlFormat
  deriving (Show, Eq)

data DiffOptions = DiffOptions
  { fromGeneration :: Maybe Text
  | toGeneration :: Maybe Text
  , showChangesOnly :: Bool
  , outputFormat :: OutputFormat
  } deriving (Show, Eq)

data DoctorOptions = DoctorOptions
  { checkAll :: Bool
  , checkConfig :: Bool
  , checkState :: Bool
  , checkModules :: Bool
  , fixIssues :: Bool
  } deriving (Show, Eq)

data LogLevel
  = Debug
  | Info
  | Warn
  | Error
  deriving (Show, Eq)

parseOptions :: IO Options
parseOptions = customExecParser prefs parser

prefs :: ParserPrefs
prefs = prefsShowHelpOnError <> prefsShowHelpOnEmpty

parser :: ParserInfo Options
parser = info (optionsParser <**> helper) descriptionMod

descriptionMod :: InfoMod Options
descriptionMod = fullDesc
  <> progDesc "A declarative configuration management system"
  <> header "mycfg - Declarative Configuration Manager"

optionsParser :: Parser Options
optionsParser = Options
  <$> commandParser
  <*> verboseParser
  <*> quietParser
  <*> jsonParser
  <*> configFileParser
  <*> stateDirParser
  <*> logLevelParser

commandParser :: Parser Command
commandParser = hsubparser
  ( command "init" (info initParser (progDesc "Initialize a new mycfg configuration"))
  <> command "apply" (info applyParser (progDesc "Apply configuration changes"))
  <> command "rollback" (info rollbackParser (progDesc "Rollback to a previous generation"))
  <> command "validate" (info validateParser (progDesc "Validate configuration"))
  <> command "list" (info listParser (progDesc "List configurations, modules, or generations"))
  <> command "diff" (info diffParser (progDesc "Show differences between configurations"))
  <> command "doctor" (info doctorParser (progDesc "Diagnose and fix configuration issues"))
  <> command "help" (info (pure Help) (progDesc "Show help information"))
  )

initParser :: Parser Command
initParser = Init <$> initOptionsParser

initOptionsParser :: Parser InitOptions
initOptionsParser = InitOptions
  <$> configPathParser
  <*> statePathParser
  <*> switch
      ( long "example"
     <> short 'e'
     <> help "Create example configuration"
      )
  <*> switch
      ( long "force"
     <> short 'f'
     <> help "Force initialization even if directory exists"
      )

applyParser :: Parser Command
applyParser = Apply <$> applyOptionsParser

applyOptionsParser :: Parser ApplyOptions
applyOptionsParser = ApplyOptions
  <$> switch
      ( long "dry-run"
     <> short 'n'
     <> help "Show what would be applied without making changes"
      )
  <*> switch
      ( long "force"
     <> short 'f'
     <> help "Force apply even if validation fails"
      )
  <*> switch
      ( long "backup"
     <> short 'b'
     <> help "Create backup before applying"
      )
  <*> switch
      ( long "validate"
     <> short 'v'
     <> help "Validate configuration before applying"
      )
  <*> switch
      ( long "continue-on-error"
     <> short 'c'
     <> help "Continue applying even if some steps fail"
      )
  <$> option auto
      ( long "max-retries"
     <> short 'r'
     <> value 3
     <> metavar "INT"
     <> help "Maximum number of retries for failed operations"
      )

rollbackParser :: Parser Command
rollbackParser = Rollback <$> rollbackOptionsParser

rollbackOptionsParser :: Parser RollbackOptions
rollbackOptionsParser = RollbackOptions
  <$> optional (textOption
      ( long "generation"
     <> short 'g'
     <> metavar "ID"
     <> help "Target generation ID for rollback"
      ))
  <*> optional (textOption
      ( long "snapshot"
     <> short 's'
     <> metavar "ID"
     <> help "Target snapshot ID for rollback"
      ))
  <*> switch
      ( long "force"
     <> short 'f'
     <> help "Force rollback even if validation fails"
      )
  <*> switch
      ( long "dry-run"
     <> short 'n'
     <> help "Show what would be rolled back without making changes"
      )
  <*> switch
      ( long "backup"
     <> short 'b'
     <> help "Create backup before rollback"
      )
  <*> switch
      ( long "validate"
     <> short 'v'
     <> help "Validate after rollback"
      )
  <*> option auto
      ( long "max-retries"
     <> short 'r'
     <> value 3
     <> metavar "INT"
     <> help "Maximum number of retries for failed operations"
      )

validateParser :: Parser Command
validateParser = Validate <$> validateOptionsParser

validateOptionsParser :: Parser ValidateOptions
validateOptionsParser = ValidateOptions
  <$> optional (fileOption
      ( long "config"
     <> short 'c'
     <> metavar "FILE"
     <> help "Configuration file to validate"
      ))
  <*> switch
      ( long "strict"
     <> short 's'
     <> help "Enable strict validation mode"
      )
  <*> switch
      ( long "warnings"
     <> short 'w'
     <> help "Show validation warnings"
      )

listParser :: Parser Command
listParser = List <$> listOptionsParser

listOptionsParser :: Parser ListOptions
listOptionsParser = ListOptions
  <$> listTypeParser
  <*> switch
      ( long "details"
     <> short 'd'
     <> help "Show detailed information"
      )
  <*> outputFormatParser

listTypeParser :: Parser ListType
listTypeParser = hsubparser
  ( command "generations" (info (pure ListGenerations) (progDesc "List configuration generations"))
  <> command "modules" (info (pure ListModules) (progDesc "List available modules"))
  <> command "profiles" (info (pure ListProfiles) (progDesc "List configuration profiles"))
  <> command "snapshots" (info (pure ListSnapshots) (progDesc "List snapshots"))
  )

outputFormatParser :: Parser OutputFormat
outputFormatParser = hsubparser
  ( command "table" (info (pure TableFormat) (progDesc "Output in table format"))
  <> command "json" (info (pure JsonFormat) (progDesc "Output in JSON format"))
  <> command "yaml" (info (pure YamlFormat) (progDesc "Output in YAML format"))
  )

diffParser :: Parser Command
diffParser = Diff <$> diffOptionsParser

diffOptionsParser :: Parser DiffOptions
diffOptionsParser = DiffOptions
  <$> optional (textOption
      ( long "from"
     <> short 'f'
     <> metavar "ID"
     <> help "From generation ID"
      ))
  <*> optional (textOption
      ( long "to"
     <> short 't'
     <> metavar "ID"
     <> help "To generation ID"
      ))
  <*> switch
      ( long "changes-only"
     <> short 'c'
     <> help "Show only changed files"
      )
  <*> outputFormatParser

doctorParser :: Parser Command
doctorParser = Doctor <$> doctorOptionsParser

doctorOptionsParser :: Parser DoctorOptions
doctorOptionsParser = DoctorOptions
  <$> switch
      ( long "all"
     <> short 'a'
     <> help "Check all aspects of the configuration"
      )
  <*> switch
      ( long "config"
     <> short 'c'
     <> help "Check configuration validity"
      )
  <*> switch
      ( long "state"
     <> short 's'
     <> help "Check state consistency"
      )
  <*> switch
      ( long "modules"
     <> short 'm'
     <> help "Check module dependencies"
      )
  <*> switch
      ( long "fix"
     <> short 'f'
     <> help "Attempt to fix detected issues"
      )

verboseParser :: Parser Bool
verboseParser = switch
  ( long "verbose"
 <> short 'v'
 <> help "Enable verbose output"
  )

quietParser :: Parser Bool
quietParser = switch
  ( long "quiet"
 <> short 'q'
 <> help "Suppress output except errors"
  )

jsonParser :: Parser Bool
jsonParser = switch
  ( long "json"
 <> short 'j'
 <> help "Output in JSON format"
  )

configFileParser :: Parser (Maybe (Path Abs File))
configFileParser = optional $ fileOption
  ( long "config"
 <> short 'c'
 <> metavar "FILE"
 <> help "Configuration file path"
  )

stateDirParser :: Parser (Maybe (Path Abs Dir))
stateDirParser = optional $ dirOption
  ( long "state-dir"
 <> short 's'
 <> metavar "DIR"
 <> help "State directory path"
  )

logLevelParser :: Parser LogLevel
logLevelParser = option readLogLevel
  ( long "log-level"
 <> short 'l'
 <> value Info
 <> metavar "LEVEL"
 <> help "Set logging level (debug, info, warn, error)"
  )

readLogLevel :: ReadM LogLevel
readLogLevel = eitherReader $ \case
  "debug" -> Right Debug
  "info" -> Right Info
  "warn" -> Right Warn
  "error" -> Right Error
  other -> Left $ "Invalid log level: " ++ other

textOption :: Mod OptionFields Text -> Parser Text
textOption = strOption . fmap Text.pack

fileOption :: Mod OptionFields (Path Abs File) -> Parser (Path Abs File)
fileOption mods = do
  pathStr <- strOption mods
  case parseAbsFile pathStr of
    Left _ -> readerError $ "Invalid file path: " ++ pathStr
    Right path -> return path

dirOption :: Mod OptionFields (Path Abs Dir) -> Parser (Path Abs Dir)
dirOption mods = do
  pathStr <- strOption mods
  case parseAbsDir pathStr of
    Left _ -> readerError $ "Invalid directory path: " ++ pathStr
    Right path -> return path

configPathParser :: Parser (Path Abs File)
configPathParser = fileOption
  ( long "config"
 <> short 'c'
 <> metavar "FILE"
 <> value "~/.config/mycfg/config.toml"
 <> help "Configuration file path"
  )

statePathParser :: Parser (Path Abs Dir)
statePathParser = dirOption
  ( long "state-dir"
 <> short 's'
 <> metavar "DIR"
 <> value "~/.local/share/mycfg"
 <> help "State directory path"
  )
