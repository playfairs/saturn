{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mycfg.Utils.Process
  ( runCommand
  , runCommandWithOutput
  , runCommandWithTimeout
  , ProcessResult(..)
  , ProcessError(..)
  , createProcess
  , waitForProcess
  , terminateProcess
  , readProcess
  , readProcessWithExitCode
  ) where

import Control.Exception (bracket, try, SomeException)
import Control.Monad (when, unless)
import Control.Concurrent (forkIO, threadDelay, MVar, newEmptyMVar, putMVar, takeMVar)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Exit (ExitCode(..))
import System.Process (CreateProcess(..), StdStream(..), createProcess, waitForProcess, terminateProcess, proc, shell)
import System.IO (Handle, hGetContents, hPutStr, hClose, hFlush)
import System.Timeout (timeout)

data ProcessResult = ProcessResult
  { exitCode :: ExitCode
  , stdout :: Text
  , stderr :: Text
  , duration :: Int
  } deriving (Show, Eq)

data ProcessError
  = ProcessTimeout
  | ProcessFailed Text
  | ProcessNotFound
  | PermissionDenied
  | InvalidCommand
  deriving (Show, Eq)

runCommand :: Text -> [Text] -> IO ProcessResult
runCommand cmd args = do
  startTime <- getCurrentTime
  let cmdStr = Text.unpack cmd
      argsStr = map Text.unpack args
      processSpec = proc cmdStr argsStr
        { std_out = CreatePipe
        , std_err = CreatePipe
        }
  
  result <- try $ createProcess processSpec
  case result of
    Left (e :: SomeException) -> 
      return $ ProcessResult (ExitFailure 1) "" "" 0
    Right (Just hIn, Just hOut, Just hErr, handle) -> do
      hClose hIn
      
      stdoutThread <- forkIO $ hGetContents hOut
      stderrThread <- forkIO $ hGetContents hErr
      
      exitCode' <- waitForProcess handle
      
      stdoutStr <- readMVar stdoutThread
      stderrStr <- readMVar stderrThread
      
      endTime <- getCurrentTime
      let duration = floor $ diffUTCTime endTime startTime
      
      return $ ProcessResult exitCode' (Text.pack stdoutStr) (Text.pack stderrStr) duration
    Right _ -> 
      return $ ProcessResult (ExitFailure 1) "" "" 0

runCommandWithOutput :: Text -> [Text] -> Text -> IO ProcessResult
runCommandWithOutput cmd args input = do
  startTime <- getCurrentTime
  let cmdStr = Text.unpack cmd
      argsStr = map Text.unpack args
      inputStr = Text.unpack input
      processSpec = proc cmdStr argsStr
        { std_out = CreatePipe
        , std_err = CreatePipe
        , std_in = CreatePipe
        }
  
  result <- try $ createProcess processSpec
  case result of
    Left (e :: SomeException) -> 
      return $ ProcessResult (ExitFailure 1) "" "" 0
    Right (Just hIn, Just hOut, Just hErr, handle) -> do
      hPutStr hIn inputStr
      hFlush hIn
      hClose hIn
      
      stdoutThread <- forkIO $ hGetContents hOut
      stderrThread <- forkIO $ hGetContents hErr
      
      exitCode' <- waitForProcess handle
      
      stdoutStr <- readMVar stdoutThread
      stderrStr <- readMVar stderrThread
      
      endTime <- getCurrentTime
      let duration = floor $ diffUTCTime endTime startTime
      
      return $ ProcessResult exitCode' (Text.pack stdoutStr) (Text.pack stderrStr) duration
    Right _ -> 
      return $ ProcessResult (ExitFailure 1) "" "" 0

runCommandWithTimeout :: Text -> [Text] -> Int -> IO (Either ProcessError ProcessResult)
runCommandWithTimeout cmd args timeoutSeconds = do
  result <- timeout (timeoutSeconds * 1000000) $ runCommand cmd args
  case result of
    Just processResult -> return $ Right processResult
    Nothing -> return $ Left ProcessTimeout

createProcess :: CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess processSpec = System.Process.createProcess processSpec

waitForProcess :: ProcessHandle -> IO ExitCode
waitForProcess = System.Process.waitForProcess

terminateProcess :: ProcessHandle -> IO ()
terminateProcess = System.Process.terminateProcess

readProcess :: Text -> [Text] -> Text -> IO ProcessResult
readProcess cmd args input = runCommandWithOutput cmd args input

readProcessWithExitCode :: Text -> [Text] -> Text -> IO (ExitCode, Text, Text)
readProcessWithExitCode cmd args input = do
  result <- readProcess cmd args input
  return (exitCode result, stdout result, stderr result)

runShellCommand :: Text -> IO ProcessResult
runShellCommand shellCmd = do
  startTime <- getCurrentTime
  let cmdStr = Text.unpack shellCmd
      processSpec = shell cmdStr
        { std_out = CreatePipe
        , std_err = CreatePipe
        }
  
  result <- try $ createProcess processSpec
  case result of
    Left (e :: SomeException) -> 
      return $ ProcessResult (ExitFailure 1) "" "" 0
    Right (Just hIn, Just hOut, Just hErr, handle) -> do
      hClose hIn
      
      stdoutThread <- forkIO $ hGetContents hOut
      stderrThread <- forkIO $ hGetContents hErr
      
      exitCode' <- waitForProcess handle
      
      stdoutStr <- readMVar stdoutThread
      stderrStr <- readMVar stderrThread
      
      endTime <- getCurrentTime
      let duration = floor $ diffUTCTime endTime startTime
      
      return $ ProcessResult exitCode' (Text.pack stdoutStr) (Text.pack stderrStr) duration
    Right _ -> 
      return $ ProcessResult (ExitFailure 1) "" "" 0

runShellCommandWithTimeout :: Text -> Int -> IO (Either ProcessError ProcessResult)
runShellCommandWithTimeout shellCmd timeoutSeconds = do
  result <- timeout (timeoutSeconds * 1000000) $ runShellCommand shellCmd
  case result of
    Just processResult -> return $ Right processResult
    Nothing -> return $ Left ProcessTimeout

captureOutput :: IO a -> IO (a, Text, Text)
captureOutput action = do
  result <- action
  return (result, "", "")

isSuccess :: ProcessResult -> Bool
isSuccess result = case exitCode result of
  ExitSuccess -> True
  ExitFailure _ -> False

isFailure :: ProcessResult -> Bool
isFailure = not . isSuccess

getExitCode :: ProcessResult -> Int
getExitCode result = case exitCode result of
  ExitSuccess -> 0
  ExitFailure code -> code

getStdout :: ProcessResult -> Text
getStdout = stdout

getStderr :: ProcessResult -> Text
getStderr = stderr

getDuration :: ProcessResult -> Int
getDuration = duration

formatProcessResult :: ProcessResult -> Text
formatProcessResult result = 
  let exitStr = case exitCode result of
        ExitSuccess -> "ExitSuccess"
        ExitFailure code -> "ExitFailure " <> Text.pack (show code)
  in "ProcessResult { exitCode = " <> exitStr <>
     ", stdout = " <> stdout result <>
     ", stderr = " <> stderr result <>
     ", duration = " <> Text.pack (show (duration result)) <>
     " }"

formatProcessError :: ProcessError -> Text
formatProcessError err = case err of
  ProcessTimeout -> "ProcessTimeout"
  ProcessFailed msg -> "ProcessFailed " <> msg
  ProcessNotFound -> "ProcessNotFound"
  PermissionDenied -> "PermissionDenied"
  InvalidCommand -> "InvalidCommand"

readMVar :: MVar a -> IO a
readMVar = takeMVar

forkIO :: IO a -> IO (MVar a)
forkIO action = do
  mvar <- newEmptyMVar
  _ <- forkIO $ do
    result <- action
    putMVar mvar result
  return mvar

getCurrentTime :: IO UTCTime
getCurrentTime = Data.Time.getCurrentTime

diffUTCTime :: UTCTime -> UTCTime -> NominalDiffTime
diffUTCTime = Data.Time.diffUTCTime

floor :: NominalDiffTime -> Int
floor = Data.Time.floor

timeout :: Int -> IO a -> IO (Maybe a)
timeout = System.Timeout.timeout

shell :: String -> CreateProcess
shell = System.Process.shell

proc :: String -> [String] -> CreateProcess
proc = System.Process.proc

CreatePipe :: StdStream
CreatePipe = System.Process.CreatePipe

ExitSuccess :: ExitCode
ExitSuccess = System.Process.ExitSuccess

ExitFailure :: Int -> ExitCode
ExitFailure = System.Process.ExitFailure

ProcessHandle :: Type
ProcessHandle = System.Process.ProcessHandle

Handle :: Type
Handle = System.IO.Handle

hGetContents :: Handle -> IO String
hGetContents = System.IO.hGetContents

hPutStr :: Handle -> String -> IO ()
hPutStr = System.IO.hPutStr

hFlush :: Handle -> IO ()
hFlush = System.IO.hFlush

hClose :: Handle -> IO ()
hClose = System.IO.hClose

forkIO :: IO () -> IO ThreadId
forkIO = Control.Concurrent.forkIO

MVar :: Type -> Type
MVar = Control.Concurrent.MVar

newEmptyMVar :: IO (MVar a)
newEmptyMVar = Control.Concurrent.newEmptyMVar

putMVar :: MVar a -> a -> IO ()
putMVar = Control.Concurrent.putMVar

takeMVar :: MVar a -> IO a
takeMVar = Control.Concurrent.takeMVar

threadDelay :: Int -> IO ()
threadDelay = Control.Concurrent.threadDelay

SomeException :: Type
SomeException = Control.Exception.SomeException

try :: IO a -> IO (Either SomeException a)
try = Control.Exception.try

timeout :: Int -> IO a -> IO (Maybe a)
timeout = System.Timeout.timeout

UTCTime :: Type
UTCTime = Data.Time.UTCTime

NominalDiffTime :: Type
NominalDiffTime = Data.Time.NominalDiffTime

getCurrentTime :: IO UTCTime
getCurrentTime = Data.Time.getCurrentTime

diffUTCTime :: UTCTime -> UTCTime -> NominalDiffTime
diffUTCTime = Data.Time.diffUTCTime

floor :: NominalDiffTime -> Int
floor = Data.Time.floor

Text :: Type
Text = Data.Text.Text

Text.pack :: String -> Text
Text.pack = Data.Text.pack

Text.unpack :: Text -> String
Text.unpack = Data.Text.unpack

Text.intercalate :: Text -> [Text] -> Text
Text.intercalate = Data.Text.intercalate

Text.show :: Show a => a -> Text
Text.show = Data.Text.pack . show

ExitCode :: Type
ExitCode = System.Exit.ExitCode

ExitSuccess :: ExitCode
ExitSuccess = System.Exit.ExitSuccess

ExitFailure :: Int -> ExitCode
ExitFailure = System.Exit.ExitFailure

ProcessHandle :: Type
ProcessHandle = System.Process.ProcessHandle

CreateProcess :: Type
CreateProcess = System.Process.CreateProcess

proc :: String -> [String] -> CreateProcess
proc = System.Process.proc

shell :: String -> CreateProcess
shell = System.Process.shell

CreatePipe :: StdStream
CreatePipe = System.Process.CreatePipe

createProcess :: CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess = System.Process.createProcess

waitForProcess :: ProcessHandle -> IO ExitCode
waitForProcess = System.Process.waitForProcess

terminateProcess :: ProcessHandle -> IO ()
terminateProcess = System.Process.terminateProcess

Handle :: Type
Handle = System.IO.Handle

hGetContents :: Handle -> IO String
hGetContents = System.IO.hGetContents

hPutStr :: Handle -> String -> IO ()
hPutStr = System.IO.hPutStr

hFlush :: Handle -> IO ()
hFlush = System.IO.hFlush

hClose :: Handle -> IO ()
hClose = System.IO.hClose

ThreadId :: Type
ThreadId = Control.Concurrent.ThreadId

forkIO :: IO () -> IO ThreadId
forkIO = Control.Concurrent.forkIO

MVar :: Type -> Type
MVar = Control.Concurrent.MVar

newEmptyMVar :: IO (MVar a)
newEmptyMVar = Control.Concurrent.newEmptyMVar

putMVar :: MVar a -> a -> IO ()
putMVar = Control.Concurrent.putMVar

takeMVar :: MVar a -> IO a
takeMVar = Control.Concurrent.takeMVar

threadDelay :: Int -> IO ()
threadDelay = Control.Concurrent.threadDelay

SomeException :: Type
SomeException = Control.Exception.SomeException

try :: IO a -> IO (Either SomeException a)
try = Control.Exception.try

timeout :: Int -> IO a -> IO (Maybe a)
timeout = System.Timeout.timeout
