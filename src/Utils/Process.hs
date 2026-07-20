{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mycfg.Utils.Process (
    runCommand,
    runCommandWithOutput,
    runCommandWithTimeout,
    ProcessResult (..),
    ProcessError (..),
    createProcess,
    waitForProcess,
    terminateProcess,
    readProcess,
    readProcessWithExitCode,
) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (unless, when)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hFlush, hGetContents, hPutStr)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, shell, terminateProcess, waitForProcess)
import System.Timeout (timeout)

data ProcessResult = ProcessResult
    { exitCode :: ExitCode
    , stdout :: Text
    , stderr :: Text
    , duration :: Int
    }
    deriving (Show, Eq)

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
        processSpec =
            proc
                cmdStr
                argsStr
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
        processSpec =
            proc
                cmdStr
                argsStr
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
        processSpec =
            shell
                cmdStr
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
     in "ProcessResult { exitCode = "
            <> exitStr
            <> ", stdout = "
            <> stdout result
            <> ", stderr = "
            <> stderr result
            <> ", duration = "
            <> Text.pack (show (duration result))
            <> " }"

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
    _ <- Control.Concurrent.forkIO $ do
        result <- action
        putMVar mvar result
    return mvar
