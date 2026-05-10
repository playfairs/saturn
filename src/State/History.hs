{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}

module Mycfg.State.History
  ( History(..)
  , HistoryEntry(..)
  , OperationType(..)
  , OperationStatus(..)
  , createHistoryEntry
  , loadHistory
  , saveHistory
  , getHistoryEntries
  , getOperationHistory
  , HistoryError(..)
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Path (Abs, File, Path, toFilePath)
import System.Directory (doesFileExist, createDirectoryIfMissing)

import Mycfg.Config.Types
import Mycfg.Errors.Types

data History = History
  { entries :: [HistoryEntry]
  , version :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON History
instance FromJSON History

data HistoryEntry = HistoryEntry
  { entryId :: UUID
  , timestamp :: UTCTime
  , operationType :: OperationType
  , operationStatus :: OperationStatus
  , generationId :: Maybe Text
  , description :: Text
  , details :: Map Text Text
  , errorMessage :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON HistoryEntry
instance FromJSON HistoryEntry

data OperationType
  = ApplyOperation
  | RollbackOperation
  | ValidationOperation
  | PlanningOperation
  | ModuleLoadOperation
  | SnapshotOperation
  | CleanupOperation
  deriving (Show, Eq, Generic)

instance ToJSON OperationType
instance FromJSON OperationType

data OperationStatus
  | Started
  | Completed
  | Failed
  | Cancelled
  deriving (Show, Eq, Generic)

instance ToJSON OperationStatus
instance FromJSON OperationStatus

data HistoryError
  = HistoryNotFound
  | HistoryCorrupted
  | HistoryWriteFailed
  | InvalidHistoryFormat
  deriving (Show, Eq)

createHistoryEntry :: OperationType -> Text -> Maybe Text -> Map Text Text -> IO HistoryEntry
createHistoryEntry opType description generationId details = do
  now <- getCurrentTime
  entryId <- nextRandom
  return $ HistoryEntry
    { entryId = entryId
    , timestamp = now
    , operationType = opType
    , operationStatus = Started
    , generationId = generationId
    , description = description
    , details = details
    , errorMessage = Nothing
    }

updateHistoryEntryStatus :: HistoryEntry -> OperationStatus -> Maybe Text -> HistoryEntry
updateHistoryEntryStatus entry status errorMsg = entry
  { operationStatus = status
  , errorMessage = errorMsg
  }

loadHistory :: Path Abs File -> IO (Either HistoryError History)
loadHistory historyPath = do
  let historyFile = toFilePath historyPath
  
  exists <- doesFileExist historyFile
  if not exists
    then return $ Right $ History [] "0.1.0"
    else do
      content <- readFile historyFile
      case decode (LazyByteString.pack content) of
        Nothing -> return $ Left HistoryCorrupted
        Just history -> return $ Right history

saveHistory :: History -> Path Abs File -> IO (Either HistoryError ())
saveHistory history historyPath = do
  let historyFile = toFilePath historyPath
      historyDir = takeDirectory historyFile
  
  createDirectoryIfMissing True historyDir
  
  let content = encode history
  result <- try $ writeFile historyFile (LazyByteString.unpack content)
  case result of
    Left (_ :: SomeException) -> return $ Left HistoryWriteFailed
    Right _ -> return $ Right ()

addHistoryEntry :: HistoryEntry -> History -> History
addHistoryEntry entry history = history
  { entries = entry : entries history }

getHistoryEntries :: History -> [HistoryEntry]
getHistoryEntries history = entries history

getOperationHistory :: OperationType -> History -> [HistoryEntry]
getOperationHistory opType history = 
  filter (\e -> operationType e == opType) (entries history)

getGenerationHistory :: Text -> History -> [HistoryEntry]
getGenerationHistory genId history = 
  filter (\e -> generationId e == Just genId) (entries history)

getRecentHistory :: Int -> History -> [HistoryEntry]
getRecentHistory count history = take count (entries history)

getFailedOperations :: History -> [HistoryEntry]
getFailedOperations history = 
  filter (\e -> operationStatus e == Failed) (entries history)

getSuccessfulOperations :: History -> [HistoryEntry]
getSuccessfulOperations history = 
  filter (\e -> operationStatus e == Completed) (entries history)

filterHistoryByTimeRange :: UTCTime -> UTCTime -> History -> [HistoryEntry]
filterHistoryByTimeRange startTime endTime history = 
  filter (\e -> timestamp e >= startTime && timestamp e <= endTime) (entries history)

searchHistoryByDescription :: Text -> History -> [HistoryEntry]
searchHistoryByDescription query history = 
  filter (\e -> query `Text.isInfixOf` description e) (entries history)

getOperationStatistics :: History -> Map OperationStatus Int
getOperationStatistics history = 
  let allEntries = entries history
      statusCounts = Map.fromListWith (+) 
        [ (operationStatus entry, 1) | entry <- allEntries ]
  in statusCounts

getOperationTypeStatistics :: History -> Map OperationType Int
getOperationTypeStatistics history = 
  let allEntries = entries history
      typeCounts = Map.fromListWith (+) 
        [ (operationType entry, 1) | entry <- allEntries ]
  in typeCounts

validateHistory :: History -> Either HistoryError ()
validateHistory history = 
  let allEntries = entries history
      hasValidEntries = all isValidEntry allEntries
  in if hasValidEntries
    then Right ()
    else Left HistoryCorrupted

isValidEntry :: HistoryEntry -> Bool
isValidEntry entry = 
  not (Text.null (description entry)) &&
  case operationStatus entry of
    Failed -> isJust (errorMessage entry)
    _ -> True

cleanupHistory :: Int -> History -> History
cleanupHistory maxEntries history = 
  let allEntries = entries history
      sortedEntries = sortBy (flip compare `on` timestamp) allEntries
      trimmedEntries = take maxEntries sortedEntries
  in history { entries = trimmedEntries }

mergeHistories :: History -> History -> History
mergeHistories h1 h2 = 
  let entries1 = entries h1
      entries2 = entries h2
      allEntries = entries1 ++ entries2
      uniqueEntries = nubBy (\e1 e2 -> entryId e1 == entryId e2) allEntries
      sortedEntries = sortBy (flip compare `on` timestamp) uniqueEntries
  in History sortedEntries "0.1.0"

exportHistory :: History -> Text
exportHistory history = 
  let allEntries = entries history
      entryLines = map formatHistoryEntry allEntries
  in Text.unlines entryLines

formatHistoryEntry :: HistoryEntry -> Text
formatHistoryEntry entry = 
  let timestampStr = Text.pack $ show (timestamp entry)
      opTypeStr = Text.pack $ show (operationType entry)
      statusStr = Text.pack $ show (operationStatus entry)
      genIdStr = case generationId entry of
        Just genId -> genId
        Nothing -> "N/A"
      errorStr = case errorMessage entry of
        Just err -> " [" <> err <> "]"
        Nothing -> ""
  in timestampStr <> " " <> opTypeStr <> " " <> statusStr <> 
     " " <> genIdStr <> " " <> description entry <> errorStr

importHistory :: Text -> Either HistoryError History
importHistory historyText = do
  let lines' = Text.lines historyText
      entries = mapMaybe parseHistoryLine lines'
  return $ History entries "0.1.0"

parseHistoryLine :: Text -> Maybe HistoryEntry
parseHistoryLine line = 
  let parts = Text.words line
  in case parts of
    [timestampStr, opTypeStr, statusStr, genIdStr, descriptionStr] -> do
      timestamp <- readMaybe (Text.unpack timestampStr)
      opType <- readMaybe (Text.unpack opTypeStr)
      status <- readMaybe (Text.unpack statusStr)
      entryId <- nextRandom
      let genId = if genIdStr == "N/A" then Nothing else Just genIdStr
      return $ HistoryEntry
        { entryId = entryId
        , timestamp = timestamp
        , operationType = opType
        , operationStatus = status
        , generationId = genId
        , description = descriptionStr
        , details = Map.empty
        , errorMessage = Nothing
        }
    _ -> Nothing
