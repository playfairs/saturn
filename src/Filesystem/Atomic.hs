{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mycfg.Filesystem.Atomic
  ( atomicWrite
  , atomicWriteText
  , atomicWriteBytes
  , atomicReplace
  , atomicMove
  , atomicCopy
  , AtomicOperation(..)
  , AtomicResult(..)
  , AtomicError(..)
  , withTempFile
  , withTempDir
  ) where

import Control.Exception (bracket, bracketOnError, try, SomeException)
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Encoding as TextEncoding
import Path (Abs, File, Dir, Path, toFilePath, parent)
import System.Directory (createDirectoryIfMissing, removeFile, renameFile, getTemporaryDirectory, doesFileExist)
import System.FilePath ((</>))
import System.IO (Handle, openTempFile, openBinaryTempFile, hClose, hPutStr, hPutStrLn, hFlush)
import System.Random (randomIO)

import Mycfg.Filesystem.Paths
import Mycfg.Errors.Types

data AtomicOperation
  = WriteFile
  | ReplaceFile
  | MoveFile
  | CopyFile
  deriving (Show, Eq)

data AtomicResult
  = AtomicSuccess
  | AtomicFailure AtomicError
  deriving (Show, Eq)

data AtomicError
  = TempFileCreationFailed
  | WriteOperationFailed
  | MoveOperationFailed
  | PermissionDenied
  | DiskSpaceError
  | TargetExists
  deriving (Show, Eq)

atomicWrite :: Path Abs File -> ByteString -> IO (Either AtomicError ())
atomicWrite targetPath content = do
  let targetFile = toFilePath targetPath
      targetDir = toFilePath (parent targetPath)
  
  result <- bracketOnError
    (createTempFile targetDir "atomic-write")
    (\(tempPath, handle) -> do
      hClose handle
      removeFile tempPath)
    (\(tempPath, handle) -> do
      BS.hPut handle content
      hFlush handle
      hClose handle
      
      renameFile tempPath targetFile
      return $ Right ())
  
  case result of
    Left (e :: SomeException) -> return $ Left WriteOperationFailed
    Right res -> return res

atomicWriteText :: Path Abs File -> Text -> IO (Either AtomicError ())
atomicWriteText targetPath content = 
  atomicWrite targetPath (TextEncoding.encodeUtf8 content)

atomicWriteBytes :: Path Abs File -> ByteString -> IO (Either AtomicError ())
atomicWriteBytes = atomicWrite

atomicReplace :: Path Abs File -> Path Abs File -> IO (Either AtomicError ())
atomicReplace sourcePath targetPath = do
  let sourceFile = toFilePath sourcePath
      targetFile = toFilePath targetPath
      targetDir = toFilePath (parent targetPath)
  
  exists <- doesFileExist targetFile
  if exists
    then return $ Left TargetExists
    else do
      result <- try $ renameFile sourceFile targetFile
      case result of
        Left (_ :: SomeException) -> return $ Left MoveOperationFailed
        Right _ -> return $ Right ()

atomicMove :: Path Abs File -> Path Abs File -> IO (Either AtomicError ())
atomicMove sourcePath targetPath = do
  let sourceFile = toFilePath sourcePath
      targetFile = toFilePath targetPath
      targetDir = toFilePath (parent targetPath)
  
  createDirectoryIfMissing True targetDir
  
  result <- try $ renameFile sourceFile targetFile
  case result of
    Left (_ :: SomeException) -> return $ Left MoveOperationFailed
    Right _ -> return $ Right ()

atomicCopy :: Path Abs File -> Path Abs File -> IO (Either AtomicError ())
atomicCopy sourcePath targetPath = do
  let sourceFile = toFilePath sourcePath
      targetFile = toFilePath targetPath
      targetDir = toFilePath (parent targetPath)
  
  createDirectoryIfMissing True targetDir
  
  result <- bracketOnError
    (openBinaryTempFile targetDir "atomic-copy")
    (\(tempPath, handle) -> do
      hClose handle
      removeFile tempPath)
    (\(tempPath, handle) -> do
      sourceContent <- BS.readFile sourceFile
      BS.hPut handle sourceContent
      hFlush handle
      hClose handle
      
      renameFile tempPath targetFile
      return $ Right ())
  
  case result of
    Left (_ :: SomeException) -> return $ Left CopyFile
    Right res -> return res

createTempFile :: FilePath -> String -> IO (FilePath, Handle)
createTempFile dir prefix = do
  tempDir <- getTemporaryDirectory
  openBinaryTempFile tempDir (prefix ++ "-temp")

withTempFile :: Path Abs Dir -> String -> ((Path Abs File, Handle) -> IO a) -> IO a
withTempFile dirPrefix prefix action = do
  let dirPath = toFilePath dirPrefix
  bracket
    (createTempFile dirPath prefix)
    (\(tempPath, handle) -> do
      hClose handle
      removeFile tempPath)
    (\(tempPath, handle) -> do
      case parseAbsFile tempPath of
        Left _ -> error "Failed to parse temp file path"
        Right tempAbsPath -> action (tempAbsPath, handle))

withTempDir :: Path Abs Dir -> String -> (Path Abs Dir -> IO a) -> IO a
withTempDir parentDir prefix action = do
  let parentPath = toFilePath parentDir
  tempDir <- getTemporaryDirectory
  randomNum <- randomIO :: IO Int
  let tempDirName = prefix ++ "-" ++ show randomNum
      fullPath = tempDir </> tempDirName
  
  bracket
    (do
      createDirectoryIfMissing True fullPath
      case parseAbsDir fullPath of
        Left _ -> error "Failed to parse temp dir path"
        Right tempAbsDir -> return tempAbsDir)
    (\tempAbsDir -> removeDirectoryRecursive (toFilePath tempAbsDir))
    action

removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive path = do
  result <- try $ removeDirectoryRecursive path
  case result of
    Left (_ :: SomeException) -> pure ()
    Right _ -> pure ()

safeAtomicWrite :: Path Abs File -> Text -> IO (Either MycfgError ())
safeAtomicWrite targetPath content = do
  result <- atomicWriteText targetPath content
  case result of
    Left err -> return $ Left $ FilesystemError $ AtomicWriteFailed (toFilePath targetPath)
    Right _ -> return $ Right ()

safeAtomicMove :: Path Abs File -> Path Abs File -> IO (Either MycfgError ())
safeAtomicMove sourcePath targetPath = do
  result <- atomicMove sourcePath targetPath
  case result of
    Left err -> return $ Left $ FilesystemError $ AtomicWriteFailed (toFilePath targetPath)
    Right _ -> return $ Right ()

safeAtomicCopy :: Path Abs File -> Path Abs File -> IO (Either MycfgError ())
safeAtomicCopy sourcePath targetPath = do
  result <- atomicCopy sourcePath targetPath
  case result of
    Left err -> return $ Left $ FilesystemError $ AtomicWriteFailed (toFilePath targetPath)
    Right _ -> return $ Right ()
