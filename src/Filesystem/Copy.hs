{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Mycfg.Filesystem.Copy (
    copyFile,
    copyFileWithPermissions,
    copyDirectory,
    copyDirectoryRecursive,
    copyOperation,
    CopyOperation (..),
    CopyResult (..),
    CopyError (..),
    safeCopyFile,
    safeCopyDirectory,
    atomicCopyFile,
) where

import Control.Exception (SomeException, bracket, bracketOnError, try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import Path (Abs, Dir, File, Path, filename, parent, toFilePath)
import System.Directory (copyFile, copyPermissions, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (Handle, IOMode (ReadMode, WriteMode), hClose, hFlush, hGetContents, hPutStr, openBinaryFile)
import System.Posix.Files (fileMode, getFileStatus, getSymbolicLinkStatus, isDirectory, isRegularFile, setFileMode)

import Mycfg.Errors.Types
import Mycfg.Filesystem.Atomic
import Mycfg.Filesystem.Paths

data CopyOperation
    = CopyFileOp
    | CopyDirectoryOp
    | CopyRecursiveOp
    | CopyWithPermissionsOp
    deriving (Show, Eq)

data CopyResult
    = CopySuccess
    | CopyFailure CopyError
    deriving (Show, Eq)

data CopyError
    = SourceNotFound
    | DestinationExists
    | PermissionDenied
    | DiskSpaceError
    | CrossDeviceError
    | InvalidPath
    deriving (Show, Eq)

copyFile :: Path Abs File -> Path Abs File -> IO (Either CopyError ())
copyFile sourcePath targetPath = do
    let sourceFile = toFilePath sourcePath
        targetFile = toFilePath targetPath
        targetDir = toFilePath (parent targetPath)

    sourceExists <- doesFileExist sourceFile
    unless sourceExists $ return $ Left SourceNotFound

    targetExists <- doesFileExist targetFile
    when targetExists $ return $ Left DestinationExists

    createDirectoryIfMissing True targetDir

    result <- try $ System.Directory.copyFile sourceFile targetFile
    case result of
        Left (_ :: SomeException) -> return $ Left PermissionDenied
        Right _ -> return $ Right ()

copyFileWithPermissions :: Path Abs File -> Path Abs File -> IO (Either CopyError ())
copyFileWithPermissions sourcePath targetPath = do
    copyResult <- copyFile sourcePath targetPath
    case copyResult of
        Left err -> return $ Left err
        Right _ -> do
            permResult <- try $ copyPermissions (toFilePath sourcePath) (toFilePath targetPath)
            case permResult of
                Left (_ :: SomeException) -> return $ Left PermissionDenied
                Right _ -> return $ Right ()

copyDirectory :: Path Abs Dir -> Path Abs Dir -> IO (Either CopyError ())
copyDirectory sourceDir targetDir = do
    let sourcePath = toFilePath sourceDir
        targetPath = toFilePath targetDir
        targetParent = toFilePath (parent targetDir)

    sourceExists <- doesDirectoryExist sourcePath
    unless sourceExists $ return $ Left SourceNotFound

    targetExists <- doesDirectoryExist targetPath
    when targetExists $ return $ Left DestinationExists

    createDirectoryIfMissing True targetParent

    result <- try $ createDirectoryIfMissing True targetPath
    case result of
        Left (_ :: SomeException) -> return $ Left PermissionDenied
        Right _ -> return $ Right ()

copyDirectoryRecursive :: Path Abs Dir -> Path Abs Dir -> IO (Either CopyError ())
copyDirectoryRecursive sourceDir targetDir = do
    let sourcePath = toFilePath sourceDir
        targetPath = toFilePath targetDir

    copyResult <- copyDirectory sourceDir targetDir
    case copyResult of
        Left err -> return $ Left err
        Right _ -> do
            entries <- try $ getDirectoryContents sourcePath
            case entries of
                Left (_ :: SomeException) -> return $ Left PermissionDenied
                Right allEntries -> do
                    let entries' = filter (`notElem` [".", ".."]) allEntries
                    copyResults <- mapM (copyEntry sourceDir targetDir) entries'
                    let failures = [err | Left err <- copyResults]
                    if null failures
                        then return $ Right ()
                        else return $ Left (head failures)

copyEntry :: Path Abs Dir -> Path Abs Dir -> FilePath -> IO (Either CopyError ())
copyEntry sourceDir targetDir entry = do
    let sourcePath = toFilePath sourceDir </> entry
        targetPath = toFilePath targetDir </> entry

    isDir <- doesDirectoryExist sourcePath
    if isDir
        then do
            case parseRelDir entry of
                Left _ -> return $ Left InvalidPath
                Right sourceRel -> do
                    case parseRelDir entry of
                        Left _ -> return $ Left InvalidPath
                        Right targetRel -> do
                            let sourceAbs = sourceDir </> sourceRel
                                targetAbs = targetDir </> targetRel
                            copyDirectoryRecursive sourceAbs targetAbs
        else do
            case parseRelFile entry of
                Left _ -> return $ Left InvalidPath
                Right sourceRel -> do
                    case parseRelFile entry of
                        Left _ -> return $ Left InvalidPath
                        Right targetRel -> do
                            let sourceAbs = sourceDir </> sourceRel
                                targetAbs = targetDir </> targetRel
                            copyFileWithPermissions sourceAbs targetAbs

copyOperation :: CopyOperation -> Path Abs File -> Path Abs File -> IO (Either CopyError ())
copyOperation op sourcePath targetPath = case op of
    CopyFileOp -> copyFile sourcePath targetPath
    CopyWithPermissionsOp -> copyFileWithPermissions sourcePath targetPath
    CopyDirectoryOp -> do
        case (parseAbsDir (toFilePath sourcePath), parseAbsDir (toFilePath targetPath)) of
            (Right sourceDir, Right targetDir) -> copyDirectory sourceDir targetDir
            _ -> return $ Left InvalidPath
    CopyRecursiveOp -> do
        case (parseAbsDir (toFilePath sourcePath), parseAbsDir (toFilePath targetPath)) of
            (Right sourceDir, Right targetDir) -> copyDirectoryRecursive sourceDir targetDir
            _ -> return $ Left InvalidPath

safeCopyFile :: Path Abs File -> Path Abs File -> IO (Either MycfgError ())
safeCopyFile sourcePath targetPath = do
    result <- copyFile sourcePath targetPath
    case result of
        Left SourceNotFound -> return $ Left $ FilesystemError $ FileNotFound (toFilePath sourcePath)
        Left DestinationExists -> return $ Left $ FilesystemError $ FileNotFound (toFilePath targetPath)
        Left PermissionDenied -> return $ Left $ FilesystemError $ PermissionDenied (toFilePath targetPath)
        Left _ -> return $ Left $ FilesystemError $ InvalidPermissions (toFilePath targetPath)
        Right _ -> return $ Right ()

safeCopyDirectory :: Path Abs Dir -> Path Abs Dir -> IO (Either MycfgError ())
safeCopyDirectory sourceDir targetDir = do
    result <- copyDirectory sourceDir targetDir
    case result of
        Left SourceNotFound -> return $ Left $ FilesystemError $ DirectoryNotFound (toFilePath sourceDir)
        Left DestinationExists -> return $ Left $ FilesystemError $ DirectoryNotFound (toFilePath targetDir)
        Left PermissionDenied -> return $ Left $ FilesystemError $ PermissionDenied (toFilePath targetDir)
        Left _ -> return $ Left $ FilesystemError $ InvalidPermissions (toFilePath targetDir)
        Right _ -> return $ Right ()

atomicCopyFile :: Path Abs File -> Path Abs File -> IO (Either MycfgError ())
atomicCopyFile sourcePath targetPath = do
    result <- atomicCopy sourcePath targetPath
    case result of
        Left err -> return $ Left $ FilesystemError $ AtomicWriteFailed (toFilePath targetPath)
        Right _ -> return $ Right ()

copyFileAtomic :: Path Abs File -> Path Abs File -> IO (Either CopyError ())
copyFileAtomic sourcePath targetPath = do
    let targetFile = toFilePath targetPath
        targetDir = toFilePath (parent targetPath)

    sourceExists <- doesFileExist (toFilePath sourcePath)
    unless sourceExists $ return $ Left SourceNotFound

    targetExists <- doesFileExist targetFile
    when targetExists $ return $ Left DestinationExists

    result <- atomicCopy sourcePath targetPath
    case result of
        Left _ -> return $ Left PermissionDenied
        Right _ -> return $ Right ()

copyWithProgress :: Path Abs File -> Path Abs File -> (Int -> IO ()) -> IO (Either CopyError ())
copyWithProgress sourcePath targetPath progressCallback = do
    let sourceFile = toFilePath sourcePath
        targetFile = toFilePath targetPath

    sourceExists <- doesFileExist sourceFile
    unless sourceExists $ return $ Left SourceNotFound

    targetExists <- doesFileExist targetFile
    when targetExists $ return $ Left DestinationExists

    result <- bracketOnError
        (openBinaryFile sourceFile ReadMode)
        hClose
        $ \sourceHandle -> do
            content <- hGetContents sourceHandle
            let contentSize = BS.length content

            bracketOnError
                (openBinaryFile targetFile WriteMode)
                hClose
                $ \targetHandle -> do
                    BS.hPut targetHandle content
                    hFlush targetHandle
                    progressCallback contentSize
                    return $ Right ()

    case result of
        Left (_ :: SomeException) -> return $ Left PermissionDenied
        Right res -> return res
