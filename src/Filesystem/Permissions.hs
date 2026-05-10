{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mycfg.Filesystem.Permissions
  ( FilePermissions(..)
  , setFilePermissions
  , getFilePermissions
  , setDirectoryPermissions
  , getDirectoryPermissions
  , copyPermissions
  , makeExecutable
  , makeWritable
  , makeReadable
  , PermissionOperation(..)
  , PermissionResult(..)
  , PermissionError(..)
  , safeSetPermissions
  , parsePermissions
  , formatPermissions
  , defaultFilePermissions
  , defaultDirectoryPermissions
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (when, unless)
import Control.Monad.IO.Class
import Data.Bits ((.|.), (.&.), complement)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead
import Path (Abs, File, Dir, Path, toFilePath)
import System.Directory (getPermissions, setPermissions, readable, writable, executable, searchable)
import System.Posix.Files (FileMode, fileMode, getFileStatus, setFileMode, ownerReadMode, ownerWriteMode, ownerExecuteMode, groupReadMode, groupWriteMode, groupExecuteMode, otherReadMode, otherWriteMode, otherExecuteMode, unionFileModes, intersectFileModes, nullFileMode)
import Text.Read (readMaybe)

import Mycfg.Errors.Types

data FilePermissions = FilePermissions
  { ownerRead :: Bool
  , ownerWrite :: Bool
  , ownerExecute :: Bool
  , groupRead :: Bool
  , groupWrite :: Bool
  , groupExecute :: Bool
  , otherRead :: Bool
  , otherWrite :: Bool
  , otherExecute :: Bool
  } deriving (Show, Eq)

data PermissionOperation
  = SetPermissions
  | GetPermissions
  | CopyPermissions
  | MakeExecutable
  | MakeWritable
  | MakeReadable
  deriving (Show, Eq)

data PermissionResult
  = PermissionSuccess FilePermissions
  | PermissionFailure PermissionError
  deriving (Show, Eq)

data PermissionError
  = PermissionDenied
  | FileNotFound
  | InvalidPermissions
  | OperationNotSupported
  deriving (Show, Eq)

defaultFilePermissions :: FilePermissions
defaultFilePermissions = FilePermissions
  { ownerRead = True
  , ownerWrite = True
  , ownerExecute = False
  , groupRead = True
  , groupWrite = False
  , groupExecute = False
  , otherRead = True
  , otherWrite = False
  , otherExecute = False
  }

defaultDirectoryPermissions :: FilePermissions
defaultDirectoryPermissions = FilePermissions
  { ownerRead = True
  , ownerWrite = True
  , ownerExecute = True
  , groupRead = True
  , groupWrite = False
  , groupExecute = True
  , otherRead = True
  , otherWrite = False
  , otherExecute = True
  }

setFilePermissions :: Path Abs File -> FilePermissions -> IO (Either PermissionError ())
setFilePermissions filePath perms = do
  let file = toFilePath filePath
      mode = permissionsToFileMode perms
  
  result <- try $ setFileMode file mode
  case result of
    Left (_ :: SomeException) -> return $ Left PermissionDenied
    Right _ -> return $ Right ()

getFilePermissions :: Path Abs File -> IO (Either PermissionError FilePermissions)
getFilePermissions filePath = do
  let file = toFilePath filePath
  
  result <- try $ getFileStatus file
  case result of
    Left (_ :: SomeException) -> return $ Left FileNotFound
    Right status -> do
      let mode = fileMode status
          perms = fileModeToPermissions mode
      return $ Right perms

setDirectoryPermissions :: Path Abs Dir -> FilePermissions -> IO (Either PermissionError ())
setDirectoryPermissions dirPath perms = do
  let dir = toFilePath dirPath
      mode = permissionsToFileMode perms
  
  result <- try $ setFileMode dir mode
  case result of
    Left (_ :: SomeException) -> return $ Left PermissionDenied
    Right _ -> return $ Right ()

getDirectoryPermissions :: Path Abs Dir -> IO (Either PermissionError FilePermissions)
getDirectoryPermissions dirPath = do
  let dir = toFilePath dirPath
  
  result <- try $ getFileStatus dir
  case result of
    Left (_ :: SomeException) -> return $ Left FileNotFound
    Right status -> do
      let mode = fileMode status
          perms = fileModeToPermissions mode
      return $ Right perms

copyPermissions :: Path Abs File -> Path Abs File -> IO (Either PermissionError ())
copyPermissions sourcePath targetPath = do
  permsResult <- getFilePermissions sourcePath
  case permsResult of
    Left err -> return $ Left err
    Right perms -> setFilePermissions targetPath perms

makeExecutable :: Path Abs File -> IO (Either PermissionError ())
makeExecutable filePath = do
  permsResult <- getFilePermissions filePath
  case permsResult of
    Left err -> return $ Left err
    Right perms -> do
      let newPerms = perms { ownerExecute = True, groupExecute = True, otherExecute = True }
      setFilePermissions filePath newPerms

makeWritable :: Path Abs File -> IO (Either PermissionError ())
makeWritable filePath = do
  permsResult <- getFilePermissions filePath
  case permsResult of
    Left err -> return $ Left err
    Right perms -> do
      let newPerms = perms { ownerWrite = True, groupWrite = True, otherWrite = True }
      setFilePermissions filePath newPerms

makeReadable :: Path Abs File -> IO (Either PermissionError ())
makeReadable filePath = do
  permsResult <- getFilePermissions filePath
  case permsResult of
    Left err -> return $ Left err
    Right perms -> do
      let newPerms = perms { ownerRead = True, groupRead = True, otherRead = True }
      setFilePermissions filePath newPerms

safeSetPermissions :: Path Abs File -> FilePermissions -> IO (Either MycfgError ())
safeSetPermissions filePath perms = do
  result <- setFilePermissions filePath perms
  case result of
    Left PermissionDenied -> return $ Left $ FilesystemError $ PermissionDenied (toFilePath filePath)
    Left FileNotFound -> return $ Left $ FilesystemError $ FileNotFound (toFilePath filePath)
    Left InvalidPermissions -> return $ Left $ FilesystemError $ InvalidPermissions (toFilePath filePath)
    Left _ -> return $ Left $ FilesystemError $ InvalidPermissions (toFilePath filePath)
    Right _ -> return $ Right ()

permissionsToFileMode :: FilePermissions -> FileMode
permissionsToFileMode perms = 
  let mode = nullFileMode
      mode1 = if ownerRead perms then mode `unionFileModes` ownerReadMode else mode
      mode2 = if ownerWrite perms then mode1 `unionFileModes` ownerWriteMode else mode1
      mode3 = if ownerExecute perms then mode2 `unionFileModes` ownerExecuteMode else mode2
      mode4 = if groupRead perms then mode3 `unionFileModes` groupReadMode else mode3
      mode5 = if groupWrite perms then mode4 `unionFileModes` groupWriteMode else mode4
      mode6 = if groupExecute perms then mode5 `unionFileModes` groupExecuteMode else mode5
      mode7 = if otherRead perms then mode6 `unionFileModes` otherReadMode else mode6
      mode8 = if otherWrite perms then mode7 `unionFileModes` otherWriteMode else mode7
      mode9 = if otherExecute perms then mode8 `unionFileModes` otherExecuteMode else mode8
  in mode9

fileModeToPermissions :: FileMode -> FilePermissions
fileModeToPermissions mode = FilePermissions
  { ownerRead = mode `intersectFileModes` ownerReadMode /= nullFileMode
  , ownerWrite = mode `intersectFileModes` ownerWriteMode /= nullFileMode
  , ownerExecute = mode `intersectFileModes` ownerExecuteMode /= nullFileMode
  , groupRead = mode `intersectFileModes` groupReadMode /= nullFileMode
  , groupWrite = mode `intersectFileModes` groupWriteMode /= nullFileMode
  , groupExecute = mode `intersectFileModes` groupExecuteMode /= nullFileMode
  , otherRead = mode `intersectFileModes` otherReadMode /= nullFileMode
  , otherWrite = mode `intersectFileModes` otherWriteMode /= nullFileMode
  , otherExecute = mode `intersectFileModes` otherExecuteMode /= nullFileMode
  }

parsePermissions :: Text -> Either Text FilePermissions
parsePermissions text = do
  case Text.unpack text of
    [r, w, x, gR, gW, gX, oR, oW, oX] -> do
      let ownerR = r == 'r'
          ownerW = w == 'w'
          ownerX = x == 'x'
          groupR = gR == 'r'
          groupW = gW == 'w'
          groupX = gX == 'x'
          otherR = oR == 'r'
          otherW = oW == 'w'
          otherX = oX == 'x'
      Right $ FilePermissions ownerR ownerW ownerX groupR groupW groupX otherR otherW otherX
    _ -> Left "Invalid permission format. Expected format: rwxrwxrwx"

formatPermissions :: FilePermissions -> Text
formatPermissions perms = Text.pack $ 
  (if ownerRead perms then "r" else "-") ++
  (if ownerWrite perms then "w" else "-") ++
  (if ownerExecute perms then "x" else "-") ++
  (if groupRead perms then "r" else "-") ++
  (if groupWrite perms then "w" else "-") ++
  (if groupExecute perms then "x" else "-") ++
  (if otherRead perms then "r" else "-") ++
  (if otherWrite perms then "w" else "-") ++
  (if otherExecute perms then "x" else "-")

parseOctalPermissions :: Text -> Either Text FilePermissions
parseOctalPermissions text = do
  case TextRead.decimal text of
    Right (octal, "") | octal >= 0 && octal <= 777 -> do
      let ownerR = octal `div` 100 >= 4
          ownerW = (octal `div` 100) `mod` 4 >= 2
          ownerX = (octal `div` 100) `mod` 2 == 1
          groupR = (octal `div` 10) `mod` 10 >= 4
          groupW = ((octal `div` 10) `mod` 10) `mod` 4 >= 2
          groupX = ((octal `div` 10) `mod` 10) `mod` 2 == 1
          otherR = octal `mod` 10 >= 4
          otherW = (octal `mod` 10) `mod` 4 >= 2
          otherX = (octal `mod` 10) `mod` 2 == 1
      Right $ FilePermissions ownerR ownerW ownerX groupR groupW groupX otherR otherW otherX
    _ -> Left "Invalid octal permission format. Expected format: 000-777"

formatOctalPermissions :: FilePermissions -> Text
formatOctalPermissions perms = 
  let owner = (if ownerRead perms then 4 else 0) + (if ownerWrite perms then 2 else 0) + (if ownerExecute perms then 1 else 0)
      group = (if groupRead perms then 4 else 0) + (if groupWrite perms then 2 else 0) + (if groupExecute perms then 1 else 0)
      other = (if otherRead perms then 4 else 0) + (if otherWrite perms then 2 else 0) + (if otherExecute perms then 1 else 0)
  in Text.pack $ show (owner * 100 + group * 10 + other)

validatePermissions :: FilePermissions -> Bool
validatePermissions perms = 
  let hasRead = ownerRead perms || groupRead perms || otherRead perms
      hasWrite = ownerWrite perms || groupWrite perms || otherWrite perms
      hasExecute = ownerExecute perms || groupExecute perms || otherExecute perms
  in hasRead || hasWrite || hasExecute
