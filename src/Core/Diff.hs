{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}

module Mycfg.Core.Diff
  ( DiffResult(..)
  , FileDiff(..)
  , DirectoryDiff(..)
  , SymlinkDiff(..)
  , DiffType(..)
  , computeDiff
  , computeManifestDiff
  , computeConfigDiff
  , applyDiff
  , DiffError(..)
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Path (Abs, File, Dir, Path, toFilePath)
import System.Directory (doesFileExist, doesDirectoryExist, getModificationTime, getFileSize)
import System.Posix.Files (getFileStatus, isSymbolicLink, readSymbolicLink)

import Mycfg.Config.Types
import Mycfg.State.Manifest
import Mycfg.Filesystem.Paths

data DiffResult = DiffResult
  { fileDiffs :: Map Text FileDiff
  , directoryDiffs :: Map Text DirectoryDiff
  , symlinkDiffs :: Map Text SymlinkDiff
  , summary :: DiffSummary
  } deriving (Show, Eq, Generic)

instance ToJSON DiffResult
instance FromJSON DiffResult

data FileDiff = FileDiff
  { diffType :: DiffType
  , sourcePath :: Maybe Text
  , targetPath :: Text
  , oldChecksum :: Maybe Text
  , newChecksum :: Maybe Text
  , oldSize :: Maybe Integer
  , newSize :: Maybe Integer
  , oldModified :: Maybe UTCTime
  , newModified :: Maybe UTCTime
  , operation :: FileOperation
  } deriving (Show, Eq, Generic)

instance ToJSON FileDiff
instance FromJSON FileDiff

data DirectoryDiff = DirectoryDiff
  { diffType :: DiffType
  , targetPath :: Text
  , oldPermissions :: Maybe Text
  , newPermissions :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON DirectoryDiff
instance FromJSON DirectoryDiff

data SymlinkDiff = SymlinkDiff
  { diffType :: DiffType
  , targetPath :: Text
  , oldTarget :: Maybe Text
  , newTarget :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON SymlinkDiff
instance FromJSON SymlinkDiff

data DiffType
  = Added
  | Removed
  | Modified
  | Unchanged
  deriving (Show, Eq, Generic)

instance ToJSON DiffType
instance FromJSON DiffType

data DiffSummary = DiffSummary
  { filesAdded :: Int
  , filesRemoved :: Int
  , filesModified :: Int
  , directoriesAdded :: Int
  , directoriesRemoved :: Int
  , symlinksAdded :: Int
  , symlinksRemoved :: Int
  , totalChanges :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON DiffSummary
instance FromJSON DiffSummary

data DiffError
  = PathNotFound Text
  | PermissionDenied Text
  | InvalidPath Text
  | DiffCalculationFailed
  deriving (Show, Eq)

computeDiff :: Config -> Maybe Manifest -> IO (Either DiffError DiffResult)
computeDiff config maybeManifest = do
  let fileMap = files config
  
  case maybeManifest of
    Nothing -> computeInitialDiff config
    Just manifest -> computeIncrementalDiff config manifest

computeInitialDiff :: Config -> IO (Either DiffError DiffResult)
computeInitialDiff config = do
  let fileMap = files config
  
  fileDiffs <- mapM computeInitialFileDiff (Map.toList fileMap)
  let fileDiffMap = Map.fromList fileDiffs
  
  dirDiffs <- computeInitialDirectoryDiffs fileMap
  let dirDiffMap = Map.fromList dirDiffs
  
  let summary = computeDiffSummary fileDiffMap dirDiffMap Map.empty
  
  return $ Right $ DiffResult fileDiffMap dirDiffMap Map.empty summary

computeIncrementalDiff :: Config -> Manifest -> IO (Either DiffError DiffResult)
computeIncrementalDiff config manifest = do
  let fileMap = files config
      manifestEntries = entries manifest
  
  fileDiffs <- mapM (computeIncrementalFileDiff manifestEntries) (Map.toList fileMap)
  let fileDiffMap = Map.fromList fileDiffs
  
  dirDiffs <- computeIncrementalDirectoryDiffs manifestEntries fileMap
  let dirDiffMap = Map.fromList dirDiffs
  
  symlinkDiffs <- computeIncrementalSymlinkDiffs manifestEntries fileMap
  let symlinkDiffMap = Map.fromList symlinkDiffs
  
  let summary = computeDiffSummary fileDiffMap dirDiffMap symlinkDiffMap
  
  return $ Right $ DiffResult fileDiffMap dirDiffMap symlinkDiffMap summary

computeInitialFileDiff :: (Text, Text) -> IO (Text, FileDiff)
computeInitialFileDiff (targetPath, sourcePath) = do
  let sourceFile = Text.unpack sourcePath
  
  exists <- doesFileExist sourceFile
  if not exists
    then return (targetPath, FileDiff Added (Just sourcePath) targetPath Nothing Nothing Nothing Nothing Nothing undefined Copy)
    else do
      size <- fromIntegral <$> getFileSize sourceFile
      modTime <- getModificationTime sourceFile
      checksum <- calculateFileChecksum sourceFile
      
      return (targetPath, FileDiff Added (Just sourcePath) targetPath Nothing (Just checksum) Nothing (Just size) Nothing (Just modTime) Copy)

computeIncrementalFileDiff :: Map Text ManifestEntry -> (Text, Text) -> IO (Text, FileDiff)
computeIncrementalFileDiff manifestEntries (targetPath, sourcePath) = do
  let sourceFile = Text.unpack sourcePath
      manifestEntry = Map.lookup targetPath manifestEntries
  
  exists <- doesFileExist sourceFile
  if not exists
    then return (targetPath, FileDiff Removed Nothing targetPath Nothing Nothing Nothing Nothing Nothing undefined Copy)
    else do
      size <- fromIntegral <$> getFileSize sourceFile
      modTime <- getModificationTime sourceFile
      checksum <- calculateFileChecksum sourceFile
      
      case manifestEntry of
        Just (FileEntry fileEntry) -> 
          let oldChecksum = Just $ Mycfg.State.Manifest.checksum fileEntry
              oldSize = Just $ Mycfg.State.Manifest.size fileEntry
              oldModified = Just $ Mycfg.State.Manifest.modifiedTime fileEntry
          in if checksum == oldChecksum && size == oldSize
            then return (targetPath, FileDiff Unchanged (Just sourcePath) targetPath oldChecksum (Just checksum) oldSize (Just size) oldModified (Just modTime) Copy)
            else return (targetPath, FileDiff Modified (Just sourcePath) targetPath oldChecksum (Just checksum) oldSize (Just size) oldModified (Just modTime) Copy)
        _ -> return (targetPath, FileDiff Added (Just sourcePath) targetPath Nothing (Just checksum) Nothing (Just size) Nothing (Just modTime) Copy)

computeInitialDirectoryDiffs :: Map Text Text -> IO [(Text, DirectoryDiff)]
computeInitialDirectoryDiffs fileMap = do
  let uniqueDirs = Set.fromList $ map (Text.pack . takeDirectory . Text.unpack . fst) (Map.keys fileMap)
      dirList = Set.toList uniqueDirs
  mapM computeInitialDirectoryDiff dirList

computeInitialDirectoryDiff :: Text -> IO (Text, DirectoryDiff)
computeInitialDirectoryDiff dirPath = do
  let dirFile = Text.unpack dirPath
  exists <- doesDirectoryExist dirFile
  if exists
    then return (dirPath, DirectoryDiff Added dirPath Nothing (Just "rwxr-xr-x"))
    else return (dirPath, DirectoryDiff Added dirPath Nothing (Just "rwxr-xr-x"))

computeIncrementalDirectoryDiffs :: Map Text ManifestEntry -> Map Text Text -> IO [(Text, DirectoryDiff)]
computeIncrementalDirectoryDiffs manifestEntries fileMap = do
  let uniqueDirs = Set.fromList $ map (Text.pack . takeDirectory . Text.unpack . fst) (Map.keys fileMap)
      dirList = Set.toList uniqueDirs
  mapM (computeIncrementalDirectoryDiff manifestEntries) dirList

computeIncrementalDirectoryDiff :: Map Text ManifestEntry -> Text -> IO (Text, DirectoryDiff)
computeIncrementalDirectoryDiff manifestEntries dirPath = do
  let dirFile = Text.unpack dirPath
      manifestEntry = Map.lookup dirPath manifestEntries
  
  exists <- doesDirectoryExist dirFile
  if not exists
    then return (dirPath, DirectoryDiff Removed dirPath Nothing Nothing)
    else do
      case manifestEntry of
        Just (DirectoryEntry dirEntry) -> 
          let oldPerms = Just $ Mycfg.State.Manifest.permissions dirEntry
          in return (dirPath, DirectoryDiff Unchanged dirPath oldPerms (Just "rwxr-xr-x"))
        _ -> return (dirPath, DirectoryDiff Added dirPath Nothing (Just "rwxr-xr-x"))

computeIncrementalSymlinkDiffs :: Map Text ManifestEntry -> Map Text Text -> IO [(Text, SymlinkDiff)]
computeIncrementalSymlinkDiffs manifestEntries fileMap = do
  let symlinkTargets = Map.filterWithKey (\targetPath sourcePath -> isSymlinkTarget targetPath) fileMap
  mapM (computeIncrementalSymlinkDiff manifestEntries) (Map.toList symlinkTargets)

computeIncrementalSymlinkDiff :: Map Text ManifestEntry -> (Text, Text) -> IO (Text, SymlinkDiff)
computeIncrementalSymlinkDiff manifestEntries (targetPath, sourcePath) = do
  let targetFile = Text.unpack targetPath
      manifestEntry = Map.lookup targetPath manifestEntries
  
  exists <- doesFileExist targetFile
  if not exists
    then return (targetPath, SymlinkDiff Removed targetPath Nothing Nothing)
    else do
      isSym <- isSymbolicLink <$> getFileStatus targetFile
      if not isSym
        then return (targetPath, SymlinkDiff Added targetPath Nothing (Just sourcePath))
        else do
          currentTarget <- readSymbolicLink targetFile
          let currentTargetText = Text.pack currentTarget
          
          case manifestEntry of
            Just (SymlinkEntry symlinkEntry) -> 
              let oldTarget = Just $ Mycfg.State.Manifest.target symlinkEntry
              in if currentTargetText == oldTarget
                then return (targetPath, SymlinkDiff Unchanged targetPath oldTarget (Just currentTargetText))
                else return (targetPath, SymlinkDiff Modified targetPath oldTarget (Just currentTargetText))
            _ -> return (targetPath, SymlinkDiff Added targetPath Nothing (Just currentTargetText))

isSymlinkTarget :: Text -> Bool
isSymlinkTarget targetPath = Text.isPrefixOf ".config/" targetPath || Text.isPrefixOf ".local/" targetPath

computeDiffSummary :: Map Text FileDiff -> Map Text DirectoryDiff -> Map Text SymlinkDiff -> DiffSummary
computeDiffSummary fileDiffs directoryDiffs symlinkDiffs = 
  let fileTypes = map diffType (Map.elems fileDiffs)
      dirTypes = map diffType (Map.elems directoryDiffs)
      symlinkTypes = map diffType (Map.elems symlinkDiffs)
      
      filesAdded = length $ filter (== Added) fileTypes
      filesRemoved = length $ filter (== Removed) fileTypes
      filesModified = length $ filter (== Modified) fileTypes
      
      directoriesAdded = length $ filter (== Added) dirTypes
      directoriesRemoved = length $ filter (== Removed) dirTypes
      
      symlinksAdded = length $ filter (== Added) symlinkTypes
      symlinksRemoved = length $ filter (== Removed) symlinkTypes
      
      totalChanges = filesAdded + filesRemoved + filesModified + 
                    directoriesAdded + directoriesRemoved + 
                    symlinksAdded + symlinksRemoved
  in DiffSummary filesAdded filesRemoved filesModified 
                   directoriesAdded directoriesRemoved
                   symlinksAdded symlinksRemoved totalChanges

computeManifestDiff :: Manifest -> Manifest -> DiffResult
computeManifestDiff oldManifest newManifest = 
  let oldEntries = entries oldManifest
      newEntries = entries newManifest
      oldPaths = Map.keysSet oldEntries
      newPaths = Map.keysSet newEntries
      
      addedPaths = Set.difference newPaths oldPaths
      removedPaths = Set.difference oldPaths newPaths
      commonPaths = Set.intersection oldPaths newPaths
      
      addedEntries = Map.restrictKeys newEntries addedPaths
      removedEntries = Map.restrictKeys oldEntries removedPaths
      commonEntries = Map.intersectionWith (,) oldEntries newEntries
      
      fileDiffs = Map.mapMaybe (manifestEntryToFileDiff Added) addedEntries
      fileDiffs' = Map.mapMaybe (manifestEntryToFileDiff Removed) removedEntries
      fileDiffs'' = Map.mapMaybe (uncurry manifestEntryToModifiedDiff) commonEntries
      
      allFileDiffs = Map.unions [fileDiffs, fileDiffs', fileDiffs'']
      directoryDiffs = Map.empty
      symlinkDiffs = Map.empty
      
      summary = computeDiffSummary allFileDiffs directoryDiffs symlinkDiffs
  in DiffResult allFileDiffs directoryDiffs symlinkDiffs summary

manifestEntryToFileDiff :: DiffType -> ManifestEntry -> Maybe FileDiff
manifestEntryToFileDiff diffType (FileEntry fileEntry) = 
  Just $ FileDiff diffType (sourcePath fileEntry) (path fileEntry) 
           Nothing (Just $ Mycfg.State.Manifest.checksum fileEntry)
           Nothing (Just $ Mycfg.State.Manifest.size fileEntry)
           Nothing (Just $ Mycfg.State.Manifest.modifiedTime fileEntry)
           (operation fileEntry)
manifestEntryToFileDiff _ _ = Nothing

manifestEntryToModifiedDiff :: (ManifestEntry, ManifestEntry) -> Maybe FileDiff
manifestEntryToModifiedDiff (FileEntry oldEntry, FileEntry newEntry) = 
  if Mycfg.State.Manifest.checksum oldEntry /= Mycfg.State.Manifest.checksum newEntry
    then Just $ FileDiff Modified (sourcePath newEntry) (path newEntry)
             (Just $ Mycfg.State.Manifest.checksum oldEntry) (Just $ Mycfg.State.Manifest.checksum newEntry)
             (Just $ Mycfg.State.Manifest.size oldEntry) (Just $ Mycfg.State.Manifest.size newEntry)
             (Just $ Mycfg.State.Manifest.modifiedTime oldEntry) (Just $ Mycfg.State.Manifest.modifiedTime newEntry)
             (operation newEntry)
    else Just $ FileDiff Unchanged (sourcePath newEntry) (path newEntry)
             (Just $ Mycfg.State.Manifest.checksum oldEntry) (Just $ Mycfg.State.Manifest.checksum newEntry)
             (Just $ Mycfg.State.Manifest.size oldEntry) (Just $ Mycfg.State.Manifest.size newEntry)
             (Just $ Mycfg.State.Manifest.modifiedTime oldEntry) (Just $ Mycfg.State.Manifest.modifiedTime newEntry)
             (operation newEntry)
manifestEntryToModifiedDiff _ _ = Nothing

computeConfigDiff :: Config -> Config -> DiffResult
computeConfigDiff oldConfig newConfig = 
  let oldFiles = files oldConfig
      newFiles = files newConfig
      
      fileDiffs = Map.mapWithKey computeConfigFileDiff (Map.unionWithKey mergeFileConfigs oldFiles newFiles)
      directoryDiffs = Map.empty
      symlinkDiffs = Map.empty
      
      summary = computeDiffSummary fileDiffs directoryDiffs symlinkDiffs
  in DiffResult fileDiffs directoryDiffs symlinkDiffs summary

computeConfigFileDiff :: Text -> Text -> FileDiff
computeConfigFileDiff targetPath sourcePath = 
  FileDiff Added (Just sourcePath) targetPath Nothing Nothing Nothing Nothing Nothing Nothing Copy

mergeFileConfigs :: Text -> Text -> Text -> Text
mergeFileConfigs _ old new = new

applyDiff :: DiffResult -> IO (Either DiffError ())
applyDiff diffResult = do
  let fileDiffs = Map.elems (fileDiffs diffResult)
      directoryDiffs = Map.elems (directoryDiffs diffResult)
      symlinkDiffs = Map.elems (symlinkDiffs diffResult)
  
  fileResults <- mapM applyFileDiff fileDiffs
  dirResults <- mapM applyDirectoryDiff directoryDiffs
  symlinkResults <- mapM applySymlinkDiff symlinkDiffs
  
  let allResults = fileResults ++ dirResults ++ symlinkResults
      errors = [err | Left err <- allResults]
  
  if null errors
    then return $ Right ()
    else return $ Left $ head errors

applyFileDiff :: FileDiff -> IO (Either DiffError ())
applyFileDiff fileDiff = case diffType fileDiff of
  Added -> do
    case sourcePath fileDiff of
      Just source -> do
        let sourceFile = Text.unpack source
            targetFile = Text.unpack (targetPath fileDiff)
        result <- copyFile sourceFile targetFile
        case result of
          Left _ -> return $ Left $ PermissionDenied (targetPath fileDiff)
          Right _ -> return $ Right ()
      Nothing -> return $ Left $ InvalidPath (targetPath fileDiff)
  Removed -> do
    let targetFile = Text.unpack (targetPath fileDiff)
    result <- removeFile targetFile
    case result of
      Left _ -> return $ Left $ PermissionDenied (targetPath fileDiff)
      Right _ -> return $ Right ()
  Modified -> do
    case sourcePath fileDiff of
      Just source -> do
        let sourceFile = Text.unpack source
            targetFile = Text.unpack (targetPath fileDiff)
        result <- copyFile sourceFile targetFile
        case result of
          Left _ -> return $ Left $ PermissionDenied (targetPath fileDiff)
          Right _ -> return $ Right ()
      Nothing -> return $ Left $ InvalidPath (targetPath fileDiff)
  Unchanged -> return $ Right ()

applyDirectoryDiff :: DirectoryDiff -> IO (Either DiffError ())
applyDirectoryDiff dirDiff = case diffType dirDiff of
  Added -> do
    let targetDir = Text.unpack (targetPath dirDiff)
    result <- createDirectoryIfMissing True targetDir
    case result of
      Left _ -> return $ Left $ PermissionDenied (targetPath dirDiff)
      Right _ -> return $ Right ()
  Removed -> do
    let targetDir = Text.unpack (targetPath dirDiff)
    result <- removeDirectoryRecursive targetDir
    case result of
      Left _ -> return $ Left $ PermissionDenied (targetPath dirDiff)
      Right _ -> return $ Right ()
  _ -> return $ Right ()

applySymlinkDiff :: SymlinkDiff -> IO (Either DiffError ())
applySymlinkDiff symlinkDiff = case diffType symlinkDiff of
  Added -> do
    case newTarget symlinkDiff of
      Just target -> do
        let targetFile = Text.unpack (targetPath symlinkDiff)
            linkTarget = Text.unpack target
        result <- createFileLink linkTarget targetFile
        case result of
          Left _ -> return $ Left $ PermissionDenied (targetPath symlinkDiff)
          Right _ -> return $ Right ()
      Nothing -> return $ Left $ InvalidPath (targetPath symlinkDiff)
  Removed -> do
    let targetFile = Text.unpack (targetPath symlinkDiff)
    result <- removeFile targetFile
    case result of
      Left _ -> return $ Left $ PermissionDenied (targetPath symlinkDiff)
      Right _ -> return $ Right ()
  Modified -> do
    case newTarget symlinkDiff of
      Just target -> do
        let targetFile = Text.unpack (targetPath symlinkDiff)
            linkTarget = Text.unpack target
        result <- removeFile targetFile
        case result of
          Left _ -> return $ Left $ PermissionDenied (targetPath symlinkDiff)
          Right _ -> do
            result' <- createFileLink linkTarget targetFile
            case result' of
              Left _ -> return $ Left $ PermissionDenied (targetPath symlinkDiff)
              Right _ -> return $ Right ()
      Nothing -> return $ Left $ InvalidPath (targetPath symlinkDiff)
  Unchanged -> return $ Right ()

calculateFileChecksum :: FilePath -> IO Text
calculateFileChecksum filePath = do
  content <- readFile filePath
  return $ Text.pack $ show $ hash content
