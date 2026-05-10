{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mycfg.Utils.Time
  ( getCurrentTime
  , formatTime
  , parseTime
  , addTime
  , diffTime
  , timeToText
  , textToTime
  , iso8601Format
  , rfc3339Format
  , humanReadableFormat
  , durationToText
  , textToDuration
  , TimeDuration(..)
  , Timestamp
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead
import Data.Time (UTCTime, NominalDiffTime, addUTCTime, diffUTCTime, getCurrentTime, formatTime, defaultTimeLocale, parseTimeM)
import Data.Time.Format.ISO8601 (iso8601Show, iso8601ParseM)
import Text.Read (readMaybe)

type Timestamp = UTCTime

data TimeDuration = TimeDuration
  { days :: Int
  , hours :: Int
  , minutes :: Int
  , seconds :: Int
  } deriving (Show, Eq)

formatTime :: String -> UTCTime -> Text
formatTime format time = 
  Text.pack $ Data.Time.Format.formatTime defaultTimeLocale format time

parseTime :: String -> Text -> Maybe UTCTime
parseTime format text = 
  parseTimeM defaultTimeLocale format (Text.unpack text)

timeToText :: UTCTime -> Text
timeToText time = Text.pack $ iso8601Show time

textToTime :: Text -> Maybe UTCTime
textToTime text = 
  case iso8601ParseM (Text.unpack text) of
    Just time -> Just time
    Nothing -> Nothing

iso8601Format :: String
iso8601Format = "%Y-%m-%dT%H:%M:%S%QZ"

rfc3339Format :: String
rfc3339Format = "%Y-%m-%dT%H:%M:%S%QZ"

humanReadableFormat :: String
humanReadableFormat = "%Y-%m-%d %H:%M:%S"

addTime :: NominalDiffTime -> UTCTime -> UTCTime
addTime = addUTCTime

diffTime :: UTCTime -> UTCTime -> NominalDiffTime
diffTime = Data.Time.diffUTCTime

durationToText :: NominalDiffTime -> Text
durationToText duration = 
  let totalSeconds = floor duration :: Int
      days' = totalSeconds `div` 86400
      hours' = (totalSeconds `mod` 86400) `div` 3600
      minutes' = (totalSeconds `mod` 3600) `div` 60
      seconds' = totalSeconds `mod` 60
  in formatDuration days' hours' minutes' seconds'

formatDuration :: Int -> Int -> Int -> Int -> Text
formatDuration d h m s = 
  let parts = []
      parts' = if d > 0 then (Text.pack (show d) <> "d") : parts else parts
      parts'' = if h > 0 then (Text.pack (show h) <> "h") : parts' else parts'
      parts''' = if m > 0 then (Text.pack (show m) <> "m") : parts'' else parts''
      parts'''' = if s > 0 then (Text.pack (show s) <> "s") : parts''' else parts'''
  in if null parts'''' then "0s" else Text.unwords parts''''

textToDuration :: Text -> Maybe NominalDiffTime
textToDuration text = 
  let textStr = Text.unpack text
      parseDuration str = case words str of
        [] -> Nothing
        parts -> parseDurationParts parts
  in parseDuration textStr

parseDurationParts :: [String] -> Maybe NominalDiffTime
parseDurationParts parts = 
  let parsePart part = 
        case readMaybe (init part) of
          Just num -> case last part of
            'd' -> Just (num * 86400)
            'h' -> Just (num * 3600)
            'm' -> Just (num * 60)
            's' -> Just num
            _ -> Nothing
          Nothing -> Nothing
      results = mapMaybe parsePart parts
  in if length results == length parts
    then Just $ fromIntegral $ sum results
    else Nothing

timeToHumanReadable :: UTCTime -> Text
timeToHumanReadable time = 
  formatTime humanReadableFormat time

parseHumanReadableTime :: Text -> Maybe UTCTime
parseHumanReadableTime text = 
  parseTime humanReadableFormat text

timeToShort :: UTCTime -> Text
timeToShort time = 
  formatTime "%Y-%m-%d %H:%M" time

parseShortTime :: Text -> Maybe UTCTime
parseShortTime text = 
  parseTime "%Y-%m-%d %H:%M" text

timeToDateOnly :: UTCTime -> Text
timeToDateOnly time = 
  formatTime "%Y-%m-%d" time

parseDateOnly :: Text -> Maybe UTCTime
parseDateOnly text = 
  parseTime "%Y-%m-%d" text

timeToTimeOnly :: UTCTime -> Text
timeToTimeOnly time = 
  formatTime "%H:%M:%S" time

parseTimeOnly :: Text -> Maybe UTCTime
parseTimeOnly text = 
  parseTime "%H:%M:%S" text

getCurrentTimestamp :: IO Timestamp
getCurrentTimestamp = getCurrentTime

timestampToUnix :: Timestamp -> Integer
timestampToUnix time = 
  floor $ diffUTCTime time (read "1970-01-01 00:00:00 UTC")

unixToTimestamp :: Integer -> Timestamp
unixToTimestamp seconds = 
  addUTCTime (fromIntegral seconds) (read "1970-01-01 00:00:00 UTC")

formatTimestamp :: Timestamp -> Text
formatTimestamp = timeToText

parseTimestamp :: Text -> Maybe Timestamp
parseTimestamp = textToTime

isRecent :: Timestamp -> NominalDiffTime -> IO Bool
isRecent time threshold = do
  now <- getCurrentTime
  return $ diffUTCTime now time <= threshold

isOlderThan :: Timestamp -> NominalDiffTime -> IO Bool
isOlderThan time threshold = do
  now <- getCurrentTime
  return $ diffUTCTime now time > threshold

age :: Timestamp -> IO NominalDiffTime
age time = do
  now <- getCurrentTime
  return $ diffUTCTime now time

formatAge :: NominalDiffTime -> Text
formatAge duration = 
  let absDuration = abs duration
      ageText = durationToText absDuration
  in if duration < 0
    then ageText <> " ago"
    else "in " <> ageText

timeRange :: Timestamp -> Timestamp -> [Timestamp]
timeRange start end = 
  let startSeconds = timestampToUnix start
      endSeconds = timestampToUnix end
  in map unixToTimestamp [startSeconds..endSeconds]

timeInRange :: Timestamp -> Timestamp -> Timestamp -> Bool
timeInRange time start end = 
  time >= start && time <= end

overlaps :: (Timestamp, Timestamp) -> (Timestamp, Timestamp) -> Bool
overlaps (start1, end1) (start2, end2) = 
  start1 <= end2 && start2 <= end1

intersection :: (Timestamp, Timestamp) -> (Timestamp, Timestamp) -> Maybe (Timestamp, Timestamp)
intersection (start1, end1) (start2, end2) = 
  let start = max start1 start2
      end = min end1 end2
  in if start <= end
    then Just (start, end)
    else Nothing

union :: (Timestamp, Timestamp) -> (Timestamp, Timestamp) -> (Timestamp, Timestamp)
union (start1, end1) (start2, end2) = 
  (min start1 start2, max end1 end2)

duration :: (Timestamp, Timestamp) -> NominalDiffTime
duration (start, end) = diffTime end start

midpoint :: (Timestamp, Timestamp) -> Timestamp
midpoint (start, end) = 
  let halfDuration = duration (start, end) / 2
  in addTime halfDuration start

contains :: (Timestamp, Timestamp) -> Timestamp -> Bool
contains (start, end) time = 
  time >= start && time <= end

expand :: (Timestamp, Timestamp) -> NominalDiffTime -> (Timestamp, Timestamp)
expand (start, end) expansion = 
  (addTime (-expansion) start, addTime expansion end)

shrink :: (Timestamp, Timestamp) -> NominalDiffTime -> (Timestamp, Timestamp)
shrink (start, end) shrinkage = 
  (addTime shrinkage start, addTime (-shrinkage) end)

isValidTimeRange :: (Timestamp, Timestamp) -> Bool
isValidTimeRange (start, end) = 
  start <= end

normalizeTimeRange :: (Timestamp, Timestamp) -> (Timestamp, Timestamp)
normalizeTimeRange (start, end) = 
  if start <= end
    then (start, end)
    else (end, start)

timeRangeToText :: (Timestamp, Timestamp) -> Text
timeRangeToText (start, end) = 
  timeToText start <> " to " <> timeToText end

textToTimeRange :: Text -> Maybe (Timestamp, Timestamp)
textToTimeRange text = 
  case Text.splitOn " to " text of
    [startText, endText] -> do
      start <- textToTime startText
      end <- textToTime endText
      return (start, end)
    _ -> Nothing

timeRangeDuration :: (Timestamp, Timestamp) -> Text
timeRangeDuration range = 
  durationToText $ duration range

timeRangeCenter :: (Timestamp, Timestamp) -> Timestamp
timeRangeCenter = midpoint

timeRangeLength :: (Timestamp, Timestamp) -> NominalDiffTime
timeRangeLength = duration

timeRangeIsEmpty :: (Timestamp, Timestamp) -> Bool
timeRangeIsEmpty (start, end) = 
  start == end

timeRangeContains :: (Timestamp, Timestamp) -> Timestamp -> Bool
timeRangeContains = contains

timeRangeOverlaps :: (Timestamp, Timestamp) -> (Timestamp, Timestamp) -> Bool
timeRangeOverlaps = overlaps

timeRangeIntersection :: (Timestamp, Timestamp) -> (Timestamp, Timestamp) -> Maybe (Timestamp, Timestamp)
timeRangeIntersection = intersection

timeRangeUnion :: (Timestamp, Timestamp) -> (Timestamp, Timestamp) -> (Timestamp, Timestamp)
timeRangeUnion = union
