{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Mycfg.Filesystem.Paths (
    TypedPath (..),
    AbsolutePath,
    RelativePath,
    HomePath,
    ConfigPath,
    StatePath,
    TempPath,
    parseAbsolutePath,
    parseRelativePath,
    parseHomePath,
    parseConfigPath,
    parseStatePath,
    parseTempPath,
    toFilePath,
    fromFilePath,
    parentDir,
    fileName,
    fileExtension,
    isAbsolute,
    isRelative,
    isChildOf,
    normalizePath,
    joinPath,
    splitPath,
    makeRelative,
    makeAbsolute,
    pathSafetyCheck,
    PathSafetyError (..),
) where

import Control.Exception (throw)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Path (Abs, Dir, File, Path, Rel, filename, parent, parseAbsDir, parseAbsFile, parseRelDir, parseRelFile, toFilePath)
import System.Directory (canonicalizePath, getCurrentDirectory, getHomeDirectory)
import System.FilePath (isAbsolute, isRelative, joinPath, makeAbsolute, makeRelative, normalise, splitDirectories, takeDirectory, takeExtension, takeFileName, (</>))

type AbsolutePath = Path Abs File
type RelativePath = Path Rel File
type HomePath = Path Abs File
type ConfigPath = Path Abs File
type StatePath = Path Abs File
type TempPath = Path Abs File

data TypedPath a = TypedPath
    { unTypedPath :: Path a File
    , pathType :: PathType
    }
    deriving (Show, Eq)

data PathType
    = Absolute
    | Relative
    | Home
    | Config
    | State
    | Temp
    deriving (Show, Eq)

data PathSafetyError
    = PathTraversalAttack
    | SymlinkLoop
    | PermissionDenied
    | PathNotFound
    | InvalidPathFormat
    deriving (Show, Eq)

parseAbsolutePath :: FilePath -> IO (Either PathSafetyError AbsolutePath)
parseAbsolutePath fp = do
    if not (isAbsolute fp)
        then return $ Left InvalidPathFormat
        else do
            canonical <- canonicalizePath fp
            case parseAbsFile canonical of
                Left _ -> return $ Left InvalidPathFormat
                Right path -> return $ Right path

parseRelativePath :: FilePath -> IO (Either PathSafetyError RelativePath)
parseRelativePath fp = do
    if isAbsolute fp
        then return $ Left InvalidPathFormat
        else do
            case parseRelFile fp of
                Left _ -> return $ Left InvalidPathFormat
                Right path -> return $ Right path

parseHomePath :: FilePath -> IO (Either PathSafetyError HomePath)
parseHomePath fp = do
    homeDir <- getHomeDirectory
    let fullPath = homeDir </> fp
    parseAbsolutePath fullPath

parseConfigPath :: FilePath -> IO (Either PathSafetyError ConfigPath)
parseConfigPath fp = parseAbsolutePath fp

parseStatePath :: FilePath -> IO (Either PathSafetyError StatePath)
parseStatePath fp = parseAbsolutePath fp

parseTempPath :: FilePath -> IO (Either PathSafetyError TempPath)
parseTempPath fp = parseAbsolutePath fp

toFilePath :: Path a b -> FilePath
toFilePath = Mycfg.Filesystem.Paths.toFilePath

fromFilePath :: FilePath -> Either String (Path Abs File)
fromFilePath = parseAbsFile

parentDir :: Path a File -> Path a Dir
parentDir = parent

fileName :: Path a File -> FilePath
fileName path = case filename path of
    Nothing -> ""
    Just name -> toFilePath name

fileExtension :: Path a File -> String
fileExtension = takeExtension . toFilePath

isChildOf :: Path Abs File -> Path Abs Dir -> Bool
isChildOf child parent =
    let childDir = parent child
     in toFilePath childDir `isPrefixOf` toFilePath parent
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

normalizePath :: FilePath -> FilePath
normalizePath = normalise

joinPath :: FilePath -> FilePath -> FilePath
joinPath = (</>)

splitPath :: FilePath -> [FilePath]
splitPath = splitDirectories

makeRelative :: FilePath -> FilePath -> FilePath
makeRelative = System.FilePath.makeRelative

makeAbsolute :: FilePath -> IO FilePath
makeAbsolute = System.FilePath.makeAbsolute

pathSafetyCheck :: FilePath -> IO (Either PathSafetyError FilePath)
pathSafetyCheck fp = do
    if ".." `elem` splitDirectories fp
        then return $ Left PathTraversalAttack
        else do
            exists <- doesPathExist fp
            if not exists
                then return $ Left PathNotFound
                else do
                    canonical <- canonicalizePath fp
                    if canonical /= normalise fp
                        then return $ Left SymlinkLoop
                        else return $ Right canonical

doesPathExist :: FilePath -> IO Bool
doesPathExist fp = do
    result <- try $ canonicalizePath fp
    case result of
        Left (_ :: SomeException) -> return False
        Right _ -> return True

safeReadFile :: Path Abs File -> IO (Either PathSafetyError Text)
safeReadFile path = do
    safety <- pathSafetyCheck (toFilePath path)
    case safety of
        Left err -> return $ Left err
        Right _ -> do
            content <- try $ TextIO.readFile (toFilePath path)
            case content of
                Left (_ :: SomeException) -> return $ Left PermissionDenied
                Right txt -> return $ Right txt

safeWriteFile :: Path Abs File -> Text -> IO (Either PathSafetyError ())
safeWriteFile path content = do
    safety <- pathSafetyCheck (toFilePath (parent path))
    case safety of
        Left err -> return $ Left err
        Right _ -> do
            result <- try $ TextIO.writeFile (toFilePath path) content
            case result of
                Left (_ :: SomeException) -> return $ Left PermissionDenied
                Right _ -> return $ Right ()
