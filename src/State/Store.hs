{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mycfg.State.Store
  ( StateStore(..)
  , StateConfig(..)
  , initializeStateStore
  , getStateStore
  , withStateStore
  , StateStoreError(..)
  , LockHandle(..)
  , acquireLock
  , releaseLock
  , withLock
  ) where

import Control.Exception (bracket, bracketOnError, try, SomeException)
import Control.Monad (when, unless)
import Control.Monad.IO.Class
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Path (Abs, Dir, File, Path, toFilePath, parent, (</>))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getHomeDirectory)
import System.FilePath ((</>))
import System.IO (Handle, openFile, IOMode(ReadWriteMode), hClose, hPutStr, hFlush)
import System.Posix.Files (fileMode, getFileStatus, setFileMode, ownerReadMode, ownerWriteMode)
import System.Random (randomIO)
import System.Timeout (timeout)

import Mycfg.Errors.Types
import Mycfg.Filesystem.Atomic
import Mycfg.Filesystem.Paths

data StateStore = StateStore
  { stateDirectory :: Path Abs Dir
  , generationsDirectory :: Path Abs Dir
  , manifestsDirectory :: Path Abs Dir
  , snapshotsDirectory :: Path Abs Dir
  , logsDirectory :: Path Abs Dir
  , cacheDirectory :: Path Abs Dir
  , lockFile :: Path Abs File
  , config :: StateConfig
  }

data StateConfig = StateConfig
  { maxGenerations :: Int
  , maxSnapshots :: Int
  , autoCleanup :: Bool
  , compressionEnabled :: Bool
  } deriving (Show, Eq)

data StateStoreError
  = StateDirectoryNotFound
  | StateDirectoryNotWritable
  | LockAcquisitionFailed
  | LockReleaseFailed
  | StateCorrupted
  deriving (Show, Eq)

data LockHandle = LockHandle
  { lockFile :: Path Abs File
  , lockContent :: Text
  }

defaultStateConfig :: StateConfig
defaultStateConfig = StateConfig
  { maxGenerations = 10
  , maxSnapshots = 50
  , autoCleanup = True
  , compressionEnabled = True
  }

initializeStateStore :: Maybe (Path Abs Dir) -> IO (Either StateStoreError StateStore)
initializeStateStore maybeStateDir = do
  stateDir <- case maybeStateDir of
    Just dir -> return dir
    Nothing -> getDefaultStateDirectory
  
  let statePath = toFilePath stateDir
  
  exists <- doesDirectoryExist statePath
  unless exists $ createDirectoryIfMissing True statePath
  
  writable <- isDirectoryWritable stateDir
  unless writable $ return $ Left StateDirectoryNotWritable
  
  let generationsDir = stateDir </> "generations"
      manifestsDir = stateDir </> "manifests"
      snapshotsDir = stateDir </> "snapshots"
      logsDir = stateDir </> "logs"
      cacheDir = stateDir </> "cache"
      lockFilePath = stateDir </> "mycfg.lock"
  
  mapM_ (createDirectoryIfMissing True) 
    [generationsDir, manifestsDir, snapshotsDir, logsDir, cacheDir]
  
  let store = StateStore
        { stateDirectory = stateDir
        , generationsDirectory = generationsDir
        , manifestsDirectory = manifestsDir
        , snapshotsDirectory = snapshotsDir
        , logsDirectory = logsDir
        , cacheDirectory = cacheDir
        , lockFile = lockFilePath
        , config = defaultStateConfig
        }
  
  return $ Right store

getStateStore :: IO (Either StateStoreError StateStore)
getStateStore = initializeStateStore Nothing

withStateStore :: (StateStore -> IO a) -> IO a
withStateStore action = do
  result <- getStateStore
  case result of
    Left err -> error $ "Failed to initialize state store: " ++ show err
    Right store -> action store

getDefaultStateDirectory :: IO (Path Abs Dir)
getDefaultStateDirectory = do
  homeDir <- getHomeDirectory
  case parseAbsDir (homeDir </> ".local" </> "share" </> "mycfg") of
    Left _ -> error "Failed to parse default state directory"
    Right dir -> return dir

isDirectoryWritable :: Path Abs Dir -> IO Bool
isDirectoryWritable dir = do
  let dirPath = toFilePath dir
  result <- try $ do
    status <- getFileStatus dirPath
    let mode = fileMode status
    return $ mode `intersectFileModes` ownerWriteMode /= nullFileMode
  case result of
    Left (_ :: SomeException) -> return False
    Right writable -> return writable

acquireLock :: StateStore -> IO (Either StateStoreError LockHandle)
acquireLock store = do
  let lockPath = lockFile store
      lockFilePath = toFilePath lockPath
  
  lockExists <- doesFileExist lockFilePath
  if lockExists
    then do
      content <- try $ TextIO.readFile lockFilePath
      case content of
        Left (_ :: SomeException) -> return $ Left LockAcquisitionFailed
        Right lockContent -> do
          if isLockStale lockContent
            then do
              result <- try $ removeFile lockFilePath
              case result of
                Left (_ :: SomeException) -> return $ Left LockAcquisitionFailed
                Right _ -> createNewLock lockPath
            else return $ Left LockAcquisitionFailed
    else createNewLock lockPath

createNewLock :: Path Abs File -> IO (Either StateStoreError LockHandle)
createNewLock lockPath = do
  lockId <- generateLockId
  let lockContent = lockId <> "\n" <> Text.pack (show =<< getCurrentTime)
  
  result <- atomicWriteText lockPath lockContent
  case result of
    Left _ -> return $ Left LockAcquisitionFailed
    Right _ -> return $ Right $ LockHandle lockPath lockContent

releaseLock :: LockHandle -> IO (Either StateStoreError ())
releaseLock lockHandle = do
  let lockPath = lockFile lockHandle
  
  result <- try $ removeFile (toFilePath lockPath)
  case result of
    Left (_ :: SomeException) -> return $ Left LockReleaseFailed
    Right _ -> return $ Right ()

withLock :: StateStore -> (StateStore -> IO a) -> IO a
withLock store action = do
  lockResult <- acquireLock store
  case lockResult of
    Left err -> error $ "Failed to acquire lock: " ++ show err
    Right lockHandle -> 
      bracket
        (return lockHandle)
        releaseLock
        (\_ -> action store)

generateLockId :: IO Text
generateLockId = do
  randomNum <- randomIO :: IO Int
  pid <- getProcessID
  return $ "lock-" <> Text.pack (show pid) <> "-" <> Text.pack (show randomNum)

isLockStale :: Text -> Bool
isLockStale lockContent = 
  case Text.lines lockContent of
    [_, timestampStr] -> do
      case readMaybe (Text.unpack timestampStr) of
        Nothing -> True
        Just timestamp -> do
          now <- getCurrentTime
          let age = diffUTCTime now timestamp
          age > 300 -- 5 minutes
    _ -> True

cleanupStateStore :: StateStore -> IO (Either StateStoreError ())
cleanupStateStore store = do
  let config' = config store
      maxGens = maxGenerations config'
      maxSnaps = maxSnapshots config'
  
  if autoCleanup config'
    then do
      genResult <- cleanupOldGenerations store maxGens
      snapResult <- cleanupOldSnapshots store maxSnaps
      case (genResult, snapResult) of
        (Right _, Right _) -> return $ Right ()
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
    else return $ Right ()

cleanupOldGenerations :: StateStore -> Int -> IO (Either StateStoreError ())
cleanupOldGenerations store maxGens = do
  let genDir = generationsDirectory store
      genPath = toFilePath genDir
  
  entries <- try $ getDirectoryContents genPath
  case entries of
    Left (_ :: SomeException) -> return $ Left StateCorrupted
    Right allEntries -> do
      let genEntries = filter (`notElem` [".", ".."]) allEntries
      if length genEntries <= maxGens
        then return $ Right ()
        else do
          let sortedEntries = sort genEntries
              toRemove = drop maxGens sortedEntries
          mapM_ removeGeneration toRemove
          return $ Right ()

cleanupOldSnapshots :: StateStore -> Int -> IO (Either StateStoreError ())
cleanupOldSnapshots store maxSnaps = do
  let snapDir = snapshotsDirectory store
      snapPath = toFilePath snapDir
  
  entries <- try $ getDirectoryContents snapPath
  case entries of
    Left (_ :: SomeException) -> return $ Left StateCorrupted
    Right allEntries -> do
      let snapEntries = filter (`notElem` [".", ".."]) allEntries
      if length snapEntries <= maxSnaps
        then return $ Right ()
        else do
          let sortedEntries = sort snapEntries
              toRemove = drop maxSnaps sortedEntries
          mapM_ removeSnapshot toRemove
          return $ Right ()

removeGeneration :: FilePath -> IO ()
removeGeneration genName = do
  result <- try $ removeDirectoryRecursive genName
  case result of
    Left (_ :: SomeException) -> pure ()
    Right _ -> pure ()

removeSnapshot :: FilePath -> IO ()
removeSnapshot snapName = do
  result <- try $ removeFile snapName
  case result of
    Left (_ :: SomeException) -> pure ()
    Right _ -> pure ()

validateStateStore :: StateStore -> IO (Either StateStoreError ())
validateStateStore store = do
  let dirs = 
        [ generationsDirectory store
        , manifestsDirectory store
        , snapshotsDirectory store
        , logsDirectory store
        , cacheDirectory store
        ]
  
  results <- mapM validateDirectory dirs
  case sequence results of
    Left err -> return $ Left err
    Right _ -> return $ Right ()

validateDirectory :: Path Abs Dir -> IO (Either StateStoreError ())
validateDirectory dir = do
  let dirPath = toFilePath dir
  
  exists <- doesDirectoryExist dirPath
  if not exists
    then return $ Left StateDirectoryNotFound
    else do
      writable <- isDirectoryWritable dir
      if writable
        then return $ Right ()
        else return $ Left StateDirectoryNotWritable
