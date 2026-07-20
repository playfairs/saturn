{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Mycfg.Filesystem.Symlink (
    createSymlink,
    removeSymlink,
    readSymlink,
    isSymlink,
    resolveSymlink,
    checkSymlinkLoop,
    SymlinkOperation (..),
    SymlinkResult (..),
    SymlinkError (..),
    safeCreateSymlink,
    safeRemoveSymlink,
    atomicSymlinkReplace,
) where

import Control.Exception (SomeException, bracket, bracketOnError, try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as Text
import Path (Abs, Dir, File, Path, parent, toFilePath)
import System.Directory (canonicalizePath, createDirectoryLink, createFileLink, doesDirectoryExist, doesFileExist, getDirectoryContents, removeFile)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (createSymbolicLink, fileMode, getFileStatus, isSymbolicLink, readSymbolicLink)
import System.Posix.Types (FileMode)

import Mycfg.Errors.Types
import Mycfg.Filesystem.Atomic
import Mycfg.Filesystem.Paths

data SymlinkOperation
    = CreateSymlink
    | RemoveSymlink
    | ReplaceSymlink
    | ReadSymlink
    deriving (Show, Eq)

data SymlinkResult
    = SymlinkSuccess
    | SymlinkFailure SymlinkError
    deriving (Show, Eq)

data SymlinkError
    = SymlinkAlreadyExists
    | SymlinkTargetNotFound
    | SymlinkLoopDetected
    | PermissionDenied
    | InvalidSymlink
    | NotASymlink
    | CrossDeviceLink
    deriving (Show, Eq)

createSymlink :: Path Abs File -> Path Abs File -> IO (Either SymlinkError ())
createSymlink targetPath linkPath = do
    let targetFile = toFilePath targetPath
        linkFile = toFilePath linkPath
        linkDir = toFilePath (parent linkPath)

    targetExists <- doesFileExist targetFile
    unless targetExists $ return $ Left SymlinkTargetNotFound

    linkExists <- doesFileExist linkFile
    when linkExists $ return $ Left SymlinkAlreadyExists

    result <- try $ createFileLink targetFile linkFile
    case result of
        Left (_ :: SomeException) -> return $ Left PermissionDenied
        Right _ -> return $ Right ()

removeSymlink :: Path Abs File -> IO (Either SymlinkError ())
removeSymlink linkPath = do
    let linkFile = toFilePath linkPath

    isLink <- isSymlink linkPath
    unless isLink $ return $ Left NotASymlink

    result <- try $ removeFile linkFile
    case result of
        Left (_ :: SomeException) -> return $ Left PermissionDenied
        Right _ -> return $ Right ()

readSymlink :: Path Abs File -> IO (Either SymlinkError (Path Abs File))
readSymlink linkPath = do
    let linkFile = toFilePath linkPath

    isLink <- isSymlink linkPath
    unless isLink $ return $ Left NotASymlink

    result <- try $ readSymbolicLink linkFile
    case result of
        Left (_ :: SomeException) -> return $ Left InvalidSymlink
        Right target -> do
            case parseAbsFile target of
                Left _ -> return $ Left InvalidSymlink
                Right targetPath -> return $ Right targetPath

isSymlink :: Path Abs File -> IO Bool
isSymlink path = do
    result <- try $ do
        status <- getFileStatus (toFilePath path)
        return $ isSymbolicLink status
    case result of
        Left (_ :: SomeException) -> return False
        Right isSym -> return isSym

resolveSymlink :: Path Abs File -> IO (Either SymlinkError (Path Abs File))
resolveSymlink symlinkPath = do
    result <- try $ canonicalizePath (toFilePath symlinkPath)
    case result of
        Left (_ :: SomeException) -> return $ Left InvalidSymlink
        Right canonicalPath -> do
            case parseAbsFile canonicalPath of
                Left _ -> return $ Left InvalidSymlink
                Right resolvedPath -> return $ Right resolvedPath

checkSymlinkLoop :: Path Abs File -> IO (Either SymlinkError ())
checkSymlinkLoop path = do
    visited <- checkLoopHelper path []
    case visited of
        Left err -> return $ Left err
        Right _ -> return $ Right ()

checkLoopHelper :: Path Abs File -> [FilePath] -> IO (Either SymlinkError [FilePath])
checkLoopHelper path visited = do
    let currentPath = toFilePath path

    if currentPath `elem` visited
        then return $ Left SymlinkLoopDetected
        else do
            isLink <- isSymlink path
            if not isLink
                then return $ Right (currentPath : visited)
                else do
                    targetResult <- readSymlink path
                    case targetResult of
                        Left err -> return $ Left err
                        Right targetPath -> checkLoopHelper targetPath (currentPath : visited)

safeCreateSymlink :: Path Abs File -> Path Abs File -> IO (Either MycfgError ())
safeCreateSymlink targetPath linkPath = do
    loopCheck <- checkSymlinkLoop linkPath
    case loopCheck of
        Left err -> return $ Left $ FilesystemError $ SymlinkLoop (toFilePath linkPath)
        Right _ -> do
            result <- createSymlink targetPath linkPath
            case result of
                Left SymlinkAlreadyExists -> return $ Left $ FilesystemError $ FileNotFound (toFilePath linkPath)
                Left SymlinkTargetNotFound -> return $ Left $ FilesystemError $ FileNotFound (toFilePath targetPath)
                Left SymlinkLoopDetected -> return $ Left $ FilesystemError $ SymlinkLoop (toFilePath linkPath)
                Left PermissionDenied -> return $ Left $ FilesystemError $ PermissionDenied (toFilePath linkPath)
                Left _ -> return $ Left $ FilesystemError $ InvalidPermissions (toFilePath linkPath)
                Right _ -> return $ Right ()

safeRemoveSymlink :: Path Abs File -> IO (Either MycfgError ())
safeRemoveSymlink linkPath = do
    result <- removeSymlink linkPath
    case result of
        Left NotASymlink -> return $ Left $ FilesystemError $ FileNotFound (toFilePath linkPath)
        Left PermissionDenied -> return $ Left $ FilesystemError $ PermissionDenied (toFilePath linkPath)
        Left _ -> return $ Left $ FilesystemError $ InvalidPermissions (toFilePath linkPath)
        Right _ -> return $ Right ()

atomicSymlinkReplace :: Path Abs File -> Path Abs File -> IO (Either MycfgError ())
atomicSymlinkReplace targetPath linkPath = do
    let linkFile = toFilePath linkPath
        linkDir = toFilePath (parent linkPath)

    exists <- doesFileExist linkFile
    if exists
        then do
            isLink <- isSymlink linkPath
            if isLink
                then do
                    tempLinkPath <- generateTempSymlinkPath linkPath
                    createResult <- createSymlink targetPath tempLinkPath
                    case createResult of
                        Left err -> return $ Left $ FilesystemError $ SymlinkLoop (toFilePath tempLinkPath)
                        Right _ -> do
                            moveResult <- atomicMove (toFilePath tempLinkPath) linkFile
                            case moveResult of
                                Left _ -> return $ Left $ FilesystemError $ AtomicWriteFailed linkFile
                                Right _ -> return $ Right ()
                else return $ Left $ FilesystemError $ FileNotFound linkFile
        else
            createSymlink targetPath linkPath >>= \case
                Left err -> return $ Left $ FilesystemError $ SymlinkLoop linkFile
                Right _ -> return $ Right ()

generateTempSymlinkPath :: Path Abs File -> IO (Path Abs File)
generateTempSymlinkPath linkPath = do
    let linkDir = parent linkPath
        baseName = takeFileName (toFilePath linkPath)
    randomNum <- randomIO :: IO Int
    let tempName = baseName ++ ".temp." ++ show randomNum
    case parseRelFile tempName of
        Left _ -> error "Failed to parse temp symlink name"
        Right tempRel -> return $ linkDir </> tempRel

validateSymlinkTarget :: Path Abs File -> IO (Either SymlinkError ())
validateSymlinkTarget targetPath = do
    exists <- doesFileExist (toFilePath targetPath)
    if exists
        then return $ Right ()
        else return $ Left SymlinkTargetNotFound
