{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}

module Mycfg.State.Snapshot
  ( Snapshot(..)
  , SnapshotEntry(..)
  , FileSnapshot(..)
  , DirectorySnapshot(..)
  , SnapshotMetadata(..)
  , createSnapshot
  , loadSnapshot
  , saveSnapshot
  , restoreSnapshot
  , compareSnapshots
  , SnapshotError(..)
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Path (Abs, File, Dir, Path, toFilePath)
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Posix.Files (getFileStatus, isSymbolicLink, readSymbolicLink, fileMode)

import Mycfg.Filesystem.Paths
import Mycfg.Filesystem.Atomic
import Mycfg.Filesystem.Copy
import Mycfg.Filesystem.Symlink

data Snapshot = Snapshot
  { metadata :: SnapshotMetadata
  , entries :: Map Text SnapshotEntry
  } deriving (Show, Eq, Generic)

instance ToJSON Snapshot
instance FromJSON Snapshot

data SnapshotEntry
  = FileSnapshot FileSnapshot
  | DirectorySnapshot DirectorySnapshot
  deriving (Show, Eq, Generic)

instance ToJSON SnapshotEntry
instance FromJSON SnapshotEntry

data FileSnapshot = FileSnapshot
  { path :: Text
  , content :: ByteString
  , permissions :: Text
  , modifiedTime :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON FileSnapshot
instance FromJSON FileSnapshot

data DirectorySnapshot = DirectorySnapshot
  { path :: Text
  , permissions :: Text
  , modifiedTime :: UTCTime
  , entries :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON DirectorySnapshot
instance FromJSON DirectorySnapshot

data SnapshotMetadata = SnapshotMetadata
  { snapshotId :: Text
  , created :: UTCTime
  , description :: Text
  , version :: Text
  , totalFiles :: Int
  , totalDirectories :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON SnapshotMetadata
instance FromJSON SnapshotMetadata

data SnapshotError
  = SnapshotNotFound
  | SnapshotCorrupted
  | SnapshotCreationFailed
  | SnapshotRestoreFailed
  | InvalidSnapshotFormat
  deriving (Show, Eq)

createSnapshot :: Text -> Text -> [Path Abs File] -> IO (Either SnapshotError Snapshot)
createSnapshot snapshotId description filePaths = do
  now <- getCurrentTime
  
  entries <- mapM createSnapshotEntry filePaths
  let entryMap = Map.fromList $ map (\e -> (getSnapshotPath e, e)) entries
      
      metadata = SnapshotMetadata
        { snapshotId = snapshotId
        , created = now
        , description = description
        , version = "0.1.0"
        , totalFiles = length [e | FileSnapshot _ <- entries]
        , totalDirectories = length [e | DirectorySnapshot _ <- entries]
        }
  
  return $ Right $ Snapshot metadata entryMap

createSnapshotEntry :: Path Abs File -> IO SnapshotEntry
createSnapshotEntry filePath = do
  let pathStr = toFilePath filePath
      pathText = Text.pack pathStr
  
  isDir <- doesDirectoryExist pathStr
  if isDir
    then createDirectorySnapshot pathText
    else createFileSnapshot filePath

createFileSnapshot :: Path Abs File -> IO SnapshotEntry
createFileSnapshot filePath = do
  let pathStr = toFilePath filePath
      pathText = Text.pack pathStr
  
  exists <- doesFileExist pathStr
  if not exists
    then return $ FileSnapshot $ FileSnapshot pathText BS.empty "" undefined
    else do
      content <- BS.readFile pathStr
      modTime <- getModificationTime pathStr
      status <- getFileStatus pathStr
      let perms = formatPermissions $ fileMode status
      
      return $ FileSnapshot $ FileSnapshot
        { path = pathText
        , content = content
        , permissions = perms
        , modifiedTime = modTime
        }

createDirectorySnapshot :: Text -> IO SnapshotEntry
createDirectorySnapshot pathText = do
  let pathStr = Text.unpack pathText
  
  exists <- doesDirectoryExist pathStr
  if not exists
    then return $ DirectorySnapshot $ DirectorySnapshot pathText "" undefined []
    else do
      modTime <- getModificationTime pathStr
      status <- getFileStatus pathStr
      let perms = formatPermissions $ fileMode status
      
      allEntries <- getDirectoryContents pathStr
      let entries' = filter (`notElem` [".", ".."]) allEntries
      
      return $ DirectorySnapshot $ DirectorySnapshot
        { path = pathText
        , permissions = perms
        , modifiedTime = modTime
        , entries = map Text.pack entries'
        }

loadSnapshot :: Path Abs File -> IO (Either SnapshotError Snapshot)
loadSnapshot snapshotPath = do
  let snapshotFile = toFilePath snapshotPath
  
  exists <- doesFileExist snapshotFile
  if not exists
    then return $ Left SnapshotNotFound
    else do
      content <- readFile snapshotFile
      case decode (LazyByteString.pack content) of
        Nothing -> return $ Left SnapshotCorrupted
        Just snapshot -> return $ Right snapshot

saveSnapshot :: Snapshot -> Path Abs File -> IO (Either SnapshotError ())
saveSnapshot snapshot snapshotPath = do
  let snapshotFile = toFilePath snapshotPath
      snapshotDir = takeDirectory snapshotFile
  
  createDirectoryIfMissing True snapshotDir
  
  let content = encode snapshot
  result <- try $ writeFile snapshotFile (LazyByteString.unpack content)
  case result of
    Left (_ :: SomeException) -> return $ Left SnapshotCreationFailed
    Right _ -> return $ Right ()

restoreSnapshot :: Snapshot -> IO (Either SnapshotError ())
restoreSnapshot snapshot = do
  let entries = Map.elems (entries snapshot)
  results <- mapM restoreSnapshotEntry entries
  let errors = [err | Left err <- results]
  if null errors
    then return $ Right ()
    else return $ Left $ head errors

restoreSnapshotEntry :: SnapshotEntry -> IO (Either SnapshotError ())
restoreSnapshotEntry entry = case entry of
  FileSnapshot fileSnap -> restoreFileSnapshot fileSnap
  DirectorySnapshot dirSnap -> restoreDirectorySnapshot dirSnap

restoreFileSnapshot :: FileSnapshot -> IO (Either SnapshotError ())
restoreFileSnapshot fileSnap = do
  let pathStr = Text.unpack (path fileSnap)
      content = content fileSnap
      perms = permissions fileSnap
  
  result <- atomicWrite (toFilePath pathStr) content
  case result of
    Left _ -> return $ Left SnapshotRestoreFailed
    Right _ -> do
      case parseAbsFile pathStr of
        Left _ -> return $ Left InvalidSnapshotFormat
        Right path -> do
          permResult <- parsePermissions perms
          case permResult of
            Left _ -> return $ Left InvalidSnapshotFormat
            Right filePerms -> do
              setPermissionsResult <- setFilePermissions path filePerms
              case setPermissionsResult of
                Left _ -> return $ Left SnapshotRestoreFailed
                Right _ -> return $ Right ()

restoreDirectorySnapshot :: DirectorySnapshot -> IO (Either SnapshotError ())
restoreDirectorySnapshot dirSnap = do
  let pathStr = Text.unpack (path dirSnap)
      perms = permissions dirSnap
  
  createDirectoryIfMissing True pathStr
  
  case parseAbsDir pathStr of
    Left _ -> return $ Left InvalidSnapshotFormat
    Right path -> do
      permResult <- parsePermissions perms
      case permResult of
        Left _ -> return $ Left InvalidSnapshotFormat
        Right dirPerms -> do
          setPermissionsResult <- setDirectoryPermissions path dirPerms
          case setPermissionsResult of
            Left _ -> return $ Left SnapshotRestoreFailed
            Right _ -> return $ Right ()

compareSnapshots :: Snapshot -> Snapshot -> SnapshotDiff
compareSnapshots oldSnapshot newSnapshot = 
  let oldEntries = entries oldSnapshot
      newEntries = entries newSnapshot
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
          Just oldEntry -> not (snapshotEntriesEqual entry oldEntry)
        ) newEntries commonPaths
  in SnapshotDiff added removed modified

data SnapshotDiff = SnapshotDiff
  { added :: Map Text SnapshotEntry
  , removed :: Map Text SnapshotEntry
  , modified :: Map Text SnapshotEntry
  } deriving (Show, Eq)

snapshotEntriesEqual :: SnapshotEntry -> SnapshotEntry -> Bool
snapshotEntriesEqual (FileSnapshot f1) (FileSnapshot f2) = 
  path f1 == path f2 && content f1 == content f2 && permissions f1 == permissions f2
snapshotEntriesEqual (DirectorySnapshot d1) (DirectorySnapshot d2) = 
  path d1 == path d2 && permissions d1 == permissions d2 && entries d1 == entries d2
snapshotEntriesEqual _ _ = False

getSnapshotPath :: SnapshotEntry -> Text
getSnapshotPath (FileSnapshot fileSnap) = path fileSnap
getSnapshotPath (DirectorySnapshot dirSnap) = path dirSnap

formatPermissions :: FileMode -> Text
formatPermissions mode = Text.pack $ show mode

parsePermissions :: Text -> Either Text FilePermissions
parsePermissions text = case Text.unpack text of
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
