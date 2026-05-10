{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}

module Mycfg.State.Manifest
  ( Manifest(..)
  , ManifestEntry(..)
  , FileEntry(..)
  , SymlinkEntry(..)
  , DirectoryEntry(..)
  , ManifestMetadata(..)
  , createManifest
  , loadManifest
  , saveManifest
  , validateManifest
  , diffManifests
  , ManifestError(..)
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Path (Abs, File, Dir, Path, toFilePath)
import System.Directory (getModificationTime, doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.Posix.Files (getFileStatus, isSymbolicLink, readSymbolicLink, fileMode)

import Mycfg.Config.Types
import Mycfg.Filesystem.Paths

data Manifest = Manifest
  { metadata :: ManifestMetadata
  , entries :: Map Text ManifestEntry
  } deriving (Show, Eq, Generic)

instance ToJSON Manifest
instance FromJSON Manifest

data ManifestEntry
  = FileEntry FileEntry
  | SymlinkEntry SymlinkEntry
  | DirectoryEntry DirectoryEntry
  deriving (Show, Eq, Generic)

instance ToJSON ManifestEntry
instance FromJSON ManifestEntry

data FileEntry = FileEntry
  { path :: Text
  , sourcePath :: Maybe Text
  , permissions :: Text
  , size :: Integer
  , checksum :: Text
  , modifiedTime :: UTCTime
  , operation :: FileOperation
  } deriving (Show, Eq, Generic)

instance ToJSON FileEntry
instance FromJSON FileEntry

data SymlinkEntry = SymlinkEntry
  { path :: Text
  , target :: Text
  , modifiedTime :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON SymlinkEntry
instance FromJSON SymlinkEntry

data DirectoryEntry = DirectoryEntry
  { path :: Text
  , permissions :: Text
  , modifiedTime :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON DirectoryEntry
instance FromJSON DirectoryEntry

data ManifestMetadata = ManifestMetadata
  { generationId :: Text
  , created :: UTCTime
  , description :: Text
  , version :: Text
  , totalFiles :: Int
  , totalDirectories :: Int
  , totalSymlinks :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON ManifestMetadata
instance FromJSON ManifestMetadata

data ManifestError
  = ManifestNotFound
  | ManifestCorrupted
  | InvalidManifestFormat
  | FileNotFound Text
  | PermissionDenied
  deriving (Show, Eq)

createManifest :: Text -> Text -> Config -> IO (Either ManifestError Manifest)
createManifest generationId description config = do
  now <- getCurrentTime
  
  fileEntries <- createFileEntries (files config)
  symlinkEntries <- createSymlinkEntries (files config)
  dirEntries <- createDirectoryEntries (files config)
  
  let allEntries = Map.fromList $ 
        map (\e -> (path $ getFileEntry e, e)) (concat [fileEntries, symlinkEntries, dirEntries])
      
      metadata = ManifestMetadata
        { generationId = generationId
        , created = now
        , description = description
        , version = "0.1.0"
        , totalFiles = length fileEntries
        , totalDirectories = length dirEntries
        , totalSymlinks = length symlinkEntries
        }
  
  return $ Right $ Manifest metadata allEntries

createFileEntries :: Map Text Text -> IO [ManifestEntry]
createFileEntries fileMap = do
  entries <- mapM createFileEntry (Map.toList fileMap)
  return $ map FileEntry entries

createFileEntry :: (Text, Text) -> IO FileEntry
createFileEntry (targetPath, sourcePath) = do
  let sourceFile = Text.unpack sourcePath
  
  exists <- doesFileExist sourceFile
  if not exists
    then return $ FileEntry targetPath (Just sourcePath) "rw-r--r--" 0 "" undefined Copy
    else do
      modTime <- getModificationTime sourceFile
      status <- getFileStatus sourceFile
      let size = fromIntegral $ fileSize status
          perms = formatPermissions $ fileMode status
      
      checksum <- calculateFileChecksum sourceFile
      
      return $ FileEntry
        { path = targetPath
        , sourcePath = Just sourcePath
        , permissions = perms
        , size = size
        , checksum = checksum
        , modifiedTime = modTime
        , operation = Copy
        }

createSymlinkEntries :: Map Text Text -> IO [ManifestEntry]
createSymlinkEntries fileMap = do
  entries <- mapM createSymlinkEntry (Map.toList fileMap)
  return $ map SymlinkEntry $ filter isJust entries
  where
    isJust (Just _) = True
    isJust Nothing = False

createSymlinkEntry :: (Text, Text) -> IO (Maybe SymlinkEntry)
createSymlinkEntry (targetPath, sourcePath) = do
  let targetFile = Text.unpack targetPath
  
  exists <- doesFileExist targetFile
  if not exists
    then return Nothing
    else do
      isSym <- isSymbolicLink <$> getFileStatus targetFile
      if not isSym
        then return Nothing
        else do
          target <- readSymbolicLink targetFile
          modTime <- getModificationTime targetFile
          return $ Just $ SymlinkEntry
            { path = targetPath
            , target = Text.pack target
            , modifiedTime = modTime
            }

createDirectoryEntries :: Map Text Text -> IO [ManifestEntry]
createDirectoryEntries fileMap = do
  let uniqueDirs = nub $ map (Text.pack . takeDirectory . Text.unpack . fst) (Map.keys fileMap)
  entries <- mapM createDirectoryEntry (filter (/= ".") uniqueDirs)
  return $ map DirectoryEntry entries

createDirectoryEntry :: Text -> IO DirectoryEntry
createDirectoryEntry dirPath = do
  let dirFile = Text.unpack dirPath
  
  exists <- doesDirectoryExist dirFile
  if not exists
    then return $ DirectoryEntry dirPath "rwxr-xr-x" undefined
    else do
      modTime <- getModificationTime dirFile
      status <- getFileStatus dirFile
      let perms = formatPermissions $ fileMode status
      
      return $ DirectoryEntry
        { path = dirPath
        , permissions = perms
        , modifiedTime = modTime
        }

loadManifest :: Path Abs File -> IO (Either ManifestError Manifest)
loadManifest manifestPath = do
  let manifestFile = toFilePath manifestPath
  
  exists <- doesFileExist manifestFile
  if not exists
    then return $ Left ManifestNotFound
    else do
      content <- readFile manifestFile
      case decode (LazyByteString.pack content) of
        Nothing -> return $ Left ManifestCorrupted
        Just manifest -> return $ Right manifest

saveManifest :: Manifest -> Path Abs File -> IO (Either ManifestError ())
saveManifest manifest manifestPath = do
  let manifestFile = toFilePath manifestPath
      manifestDir = takeDirectory manifestFile
  
  createDirectoryIfMissing True manifestDir
  
  let content = encode manifest
  result <- try $ writeFile manifestFile (LazyByteString.unpack content)
  case result of
    Left (_ :: SomeException) -> return $ Left PermissionDenied
    Right _ -> return $ Right ()

validateManifest :: Manifest -> IO (Either ManifestError ())
validateManifest manifest = do
  let entries = Map.elems (entries manifest)
      fileEntries = [e | FileEntry e <- entries]
      symlinkEntries = [e | SymlinkEntry e <- entries]
      dirEntries = [e | DirectoryEntry e <- entries]
  
  fileValidation <- validateFileEntries fileEntries
  symlinkValidation <- validateSymlinkEntries symlinkEntries
  dirValidation <- validateDirectoryEntries dirEntries
  
  case (fileValidation, symlinkValidation, dirValidation) of
    (Right _, Right _, Right _) -> return $ Right ()
    (Left err, _, _) -> return $ Left err
    (_, Left err, _) -> return $ Left err
    (_, _, Left err) -> return $ Left err

validateFileEntries :: [FileEntry] -> IO (Either ManifestError ())
validateFileEntries entries = do
  results <- mapM validateFileEntry entries
  let errors = [err | Left err <- results]
  if null errors
    then return $ Right ()
    else return $ Left $ head errors

validateFileEntry :: FileEntry -> IO (Either ManifestError ())
validateFileEntry entry = do
  let targetFile = Text.unpack (path entry)
  
  exists <- doesFileExist targetFile
  if not exists
    then return $ Left $ FileNotFound (path entry)
    else return $ Right ()

validateSymlinkEntries :: [SymlinkEntry] -> IO (Either ManifestError ())
validateSymlinkEntries entries = do
  results <- mapM validateSymlinkEntry entries
  let errors = [err | Left err <- results]
  if null errors
    then return $ Right ()
    else return $ Left $ head errors

validateSymlinkEntry :: SymlinkEntry -> IO (Either ManifestError ())
validateSymlinkEntry entry = do
  let targetFile = Text.unpack (path entry)
  
  exists <- doesFileExist targetFile
  if not exists
    then return $ Left $ FileNotFound (path entry)
    else do
      isSym <- isSymbolicLink <$> getFileStatus targetFile
      if not isSym
        then return $ Left InvalidManifestFormat
        else return $ Right ()

validateDirectoryEntries :: [DirectoryEntry] -> IO (Either ManifestError ())
validateDirectoryEntries entries = do
  results <- mapM validateDirectoryEntry entries
  let errors = [err | Left err <- results]
  if null errors
    then return $ Right ()
    else return $ Left $ head errors

validateDirectoryEntry :: DirectoryEntry -> IO (Either ManifestError ())
validateDirectoryEntry entry = do
  let dirFile = Text.unpack (path entry)
  
  exists <- doesDirectoryExist dirFile
  if not exists
    then return $ Left $ FileNotFound (path entry)
    else return $ Right ()

diffManifests :: Manifest -> Manifest -> ManifestDiff
diffManifests oldManifest newManifest = 
  let oldEntries = entries oldManifest
      newEntries = entries newManifest
      oldPaths = Map.keysSet oldEntries
      newPaths = Map.keysSet newEntries
      
      addedPaths = Set.difference newPaths oldPaths
      removedPaths = Set.difference oldPaths newPaths
      commonPaths = Set.intersection oldPaths newPaths
      
      added = Map.restrictKeys newEntries addedPaths
      removed = Map.restrictKeys oldEntries removedPaths
      modified = Map.filterWithKey (\path entry -> 
        case Map.lookup path oldEntries of
          Nothing -> False
          Just oldEntry -> entry /= oldEntry
        ) newEntries commonPaths
  in ManifestDiff added removed modified

data ManifestDiff = ManifestDiff
  { added :: Map Text ManifestEntry
  , removed :: Map Text ManifestEntry
  , modified :: Map Text ManifestEntry
  } deriving (Show, Eq)

calculateFileChecksum :: FilePath -> IO Text
calculateFileChecksum filePath = do
  content <- readFile filePath
  return $ Text.pack $ show $ hash content

formatPermissions :: FileMode -> Text
formatPermissions mode = Text.pack $ show mode

getFileEntry :: ManifestEntry -> FileEntry
getFileEntry (FileEntry entry) = entry
getFileEntry _ = error "Not a file entry"
