{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mycfg.Logging.Logger
  ( Logger(..)
  , LogLevel(..)
  , LogEvent(..)
  , initLogger
  , logEvent
  , logDebug
  , logInfo
  , logWarn
  , logError
  , withOperation
  , createOperationId
  , OperationContext(..)
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Time (getCurrentTime, UTCTime)
import System.IO (Handle, stdout, stderr)
import System.Random (randomIO)

import Mycfg.Logging.Events
import Mycfg.Logging.Format

data Logger = Logger
  { logLevel :: LogLevel
  , logQueue :: TQueue LogEvent
  , logHandle :: Handle
  , errHandle :: Handle
  , jsonOutput :: Bool
  }

data OperationContext = OperationContext
  { operationId :: Text
  , logger :: Logger
  }

initLogger :: IO Logger
initLogger = do
  queue <- newTQueueIO
  return $ Logger
    { logLevel = Info
    , logQueue = queue
    , logHandle = stdout
    , errHandle = stderr
    , jsonOutput = False
    }

createOperationId :: IO Text
createOperationId = do
  randomNum <- randomIO :: IO Int
  return $ "op-" <> Text.pack (show randomNum)

withOperation :: Logger -> (OperationContext -> IO a) -> IO a
withOperation logger action = do
  opId <- createOperationId
  let ctx = OperationContext opId logger
  action ctx

logEvent :: Logger -> LogEvent -> IO ()
logEvent logger event = do
  let level = getLogLevel event
  if level >= logLevel logger
    then do
      atomically $ writeTQueue (logQueue logger) event
      let formatted = if jsonOutput logger
            then formatEventJson event
            else Text.pack $ show $ formatEvent event
      
      if level >= Error
        then TextIO.hPutStrLn (errHandle logger) formatted
        else TextIO.hPutStrLn (logHandle logger) formatted
    else pure ()

logDebug :: OperationContext -> Text -> IO a -> IO a
logDebug ctx message action = do
  now <- getCurrentTime
  logEvent (logger ctx) $ CustomEvent
    { operationId = operationId ctx
    , timestamp = now
    , level = Debug
    , category = "debug"
    , message = message
    , details = []
    }
  action

logInfo :: OperationContext -> Text -> IO a -> IO a
logInfo ctx message action = do
  now <- getCurrentTime
  logEvent (logger ctx) $ CustomEvent
    { operationId = operationId ctx
    , timestamp = now
    , level = Info
    , category = "info"
    , message = message
    , details = []
    }
  action

logWarn :: OperationContext -> Text -> IO a -> IO a
logWarn ctx message action = do
  now <- getCurrentTime
  logEvent (logger ctx) $ CustomEvent
    { operationId = operationId ctx
    , timestamp = now
    , level = Warn
    , category = "warning"
    , message = message
    , details = []
    }
  action

logError :: OperationContext -> Text -> IO a -> IO a
logError ctx message action = do
  now <- getCurrentTime
  logEvent (logger ctx) $ CustomEvent
    { operationId = operationId ctx
    , timestamp = now
    , level = Error
    , category = "error"
    , message = message
    , details = []
    }
  action

logSystemStarted :: OperationContext -> Text -> IO ()
logSystemStarted ctx version = do
  now <- getCurrentTime
  logEvent (logger ctx) $ SystemStarted
    { operationId = operationId ctx
    , timestamp = now
    , version = version
    }

logSystemShutdown :: OperationContext -> IO ()
logSystemShutdown ctx = do
  now <- getCurrentTime
  logEvent (logger ctx) $ SystemShutdown
    { operationId = operationId ctx
    , timestamp = now
    }

logConfigLoaded :: OperationContext -> Text -> [Text] -> IO ()
logConfigLoaded ctx configPath modules = do
  now <- getCurrentTime
  logEvent (logger ctx) $ ConfigLoaded
    { operationId = operationId ctx
    , timestamp = now
    , configPath = configPath
    , modules = modules
    }

logConfigParseFailed :: OperationContext -> Text -> Text -> IO ()
logConfigParseFailed ctx configPath error = do
  now <- getCurrentTime
  logEvent (logger ctx) $ ConfigParseFailed
    { operationId = operationId ctx
    , timestamp = now
    , configPath = configPath
    , error = error
    }

logValidationStarted :: OperationContext -> IO ()
logValidationStarted ctx = do
  now <- getCurrentTime
  logEvent (logger ctx) $ ValidationStarted
    { operationId = operationId ctx
    , timestamp = now
    }

logValidationCompleted :: OperationContext -> [Text] -> IO ()
logValidationCompleted ctx warnings = do
  now <- getCurrentTime
  logEvent (logger ctx) $ ValidationCompleted
    { operationId = operationId ctx
    , timestamp = now
    , warnings = warnings
    }

logValidationFailed :: OperationContext -> [Text] -> IO ()
logValidationFailed ctx errors = do
  now <- getCurrentTime
  logEvent (logger ctx) $ ValidationFailed
    { operationId = operationId ctx
    , timestamp = now
    , errors = errors
    }

logModuleLoaded :: OperationContext -> Text -> [Text] -> IO ()
logModuleLoaded ctx moduleName dependencies = do
  now <- getCurrentTime
  logEvent (logger ctx) $ ModuleLoaded
    { operationId = operationId ctx
    , timestamp = now
    , moduleName = moduleName
    , dependencies = dependencies
    }

logModuleLoadFailed :: OperationContext -> Text -> Text -> IO ()
logModuleLoadFailed ctx moduleName error = do
  now <- getCurrentTime
  logEvent (logger ctx) $ ModuleLoadFailed
    { operationId = operationId ctx
    , timestamp = now
    , moduleName = moduleName
    , error = error
    }

logApplyStarted :: OperationContext -> Bool -> IO ()
logApplyStarted ctx dryRun = do
  now <- getCurrentTime
  logEvent (logger ctx) $ ApplyStarted
    { operationId = operationId ctx
    , timestamp = now
    , dryRun = dryRun
    }

logApplyCompleted :: OperationContext -> Int -> Bool -> IO ()
logApplyCompleted ctx operationsApplied dryRun = do
  now <- getCurrentTime
  logEvent (logger ctx) $ ApplyCompleted
    { operationId = operationId ctx
    , timestamp = now
    , operationsApplied = operationsApplied
    , dryRun = dryRun
    }

logApplyFailed :: OperationContext -> Text -> Int -> IO ()
logApplyFailed ctx error partialOperations = do
  now <- getCurrentTime
  logEvent (logger ctx) $ ApplyFailed
    { operationId = operationId ctx
    , timestamp = now
    , error = error
    , partialOperations = partialOperations
    }

logFileOperation :: OperationContext -> Text -> Text -> Text -> Bool -> IO ()
logFileOperation ctx operation sourcePath targetPath success = do
  now <- getCurrentTime
  logEvent (logger ctx) $ FileOperation
    { operationId = operationId ctx
    , timestamp = now
    , operation = operation
    , sourcePath = sourcePath
    , targetPath = targetPath
    , success = success
    }

logGenerationCreated :: OperationContext -> Text -> Text -> IO ()
logGenerationCreated ctx generationId description = do
  now <- getCurrentTime
  logEvent (logger ctx) $ GenerationCreated
    { operationId = operationId ctx
    , timestamp = now
    , generationId = generationId
    , description = description
    }

logGenerationActivated :: OperationContext -> Text -> IO ()
logGenerationActivated ctx generationId = do
  now <- getCurrentTime
  logEvent (logger ctx) $ GenerationActivated
    { operationId = operationId ctx
    , timestamp = now
    , generationId = generationId
    }

logRollbackStarted :: OperationContext -> Text -> IO ()
logRollbackStarted ctx generationId = do
  now <- getCurrentTime
  logEvent (logger ctx) $ RollbackStarted
    { operationId = operationId ctx
    , timestamp = now
    , generationId = generationId
    }

logRollbackCompleted :: OperationContext -> Int -> IO ()
logRollbackCompleted ctx operationsReverted = do
  now <- getCurrentTime
  logEvent (logger ctx) $ RollbackCompleted
    { operationId = operationId ctx
    , timestamp = now
    , operationsReverted = operationsReverted
    }

logRollbackFailed :: OperationContext -> Text -> IO ()
logRollbackFailed ctx error = do
  now <- getCurrentTime
  logEvent (logger ctx) $ RollbackFailed
    { operationId = operationId ctx
    , timestamp = now
    , error = error
    }
