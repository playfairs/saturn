{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mycfg.Utils.Text
  ( stripPrefix
  , stripSuffix
  , splitOn
  , joinWith
  , capitalize
  , uncapitalize
  , camelToSnake
  , snakeToCamel
  , escapeShell
  , unescapeShell
  , truncateText
  , padText
  , wrapText
  , indentText
  , normalizeSpaces
  , isBlank
  , nonEmpty
  , safeHead
  , safeLast
  , safeInit
  , safeTail
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Char (toUpper, toLower, isSpace)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.List as List

stripPrefix :: Text -> Text -> Maybe Text
stripPrefix prefix text = 
  let prefixStr = Text.unpack prefix
      textStr = Text.unpack text
  in if prefixStr `isPrefixOf` textStr
    then Just $ Text.pack $ drop (length prefixStr) textStr
    else Nothing

stripSuffix :: Text -> Text -> Maybe Text
stripSuffix suffix text = 
  let suffixStr = Text.unpack suffix
      textStr = Text.unpack text
  in if suffixStr `isSuffixOf` textStr
    then Just $ Text.pack $ take (length textStr - length suffixStr) textStr
    else Nothing

splitOn :: Text -> Text -> [Text]
splitOn sep text = 
  let sepStr = Text.unpack sep
      textStr = Text.unpack text
      parts = List.splitOn sepStr textStr
  in map Text.pack parts

joinWith :: Text -> [Text] -> Text
joinWith sep texts = 
  let sepStr = Text.unpack sep
      textsStr = map Text.unpack texts
  in Text.pack $ List.intercalate sepStr textsStr

capitalize :: Text -> Text
capitalize text = 
  case Text.uncons text of
    Nothing -> text
    Just (first, rest) -> Text.cons (toUpper first) rest

uncapitalize :: Text -> Text
uncapitalize text = 
  case Text.uncons text of
    Nothing -> text
    Just (first, rest) -> Text.cons (toLower first) rest

camelToSnake :: Text -> Text
camelToSnake text = 
  let chars = Text.unpack text
      result = concatMap (\c -> if isUpper c then '_' : toLower c else [c]) chars
  in Text.pack result

snakeToCamel :: Text -> Text
snakeToCamel text = 
  let parts = splitOn "_" text
      capitalized = map capitalize parts
  case parts of
    [] -> text
    (first:rest) -> Text.concat (first : map uncapitalize rest)

escapeShell :: Text -> Text
escapeShell text = 
  let textStr = Text.unpack text
      needsEscape = any (`elem` " \t\n\r'\"`$&*()[]{}|;<>?") textStr
  if needsEscape
    then Text.pack $ "'" ++ concatMap escapeChar textStr ++ "'"
    else text
  where
    escapeChar '\'' = "'\"'\"'"
    escapeChar c = [c]

unescapeShell :: Text -> Text
unescapeShell text = 
  let textStr = Text.unpack text
  in case textStr of
    '\'' : rest -> 
      case List.stripSuffix "'" rest of
        Just content -> Text.pack $ unescapeContent content
        Nothing -> text
    _ -> text
  where
    unescapeContent = concatMap unescapeChar
    unescapeChar '\'' = '\''
    unescapeChar '"' = '"'
    unescapeChar c = [c]

truncateText :: Int -> Text -> Text
truncateText maxLen text = 
  if Text.length text <= maxLen
    then text
    else Text.take (maxLen - 3) text <> "..."

padText :: Int -> Text -> Text
padText width text = 
  let len = Text.length text
      padding = replicate (width - len) ' '
  in text <> Text.pack padding

wrapText :: Int -> Text -> Text
wrapText width text = 
  let words = Text.words text
      wrapWords [] acc = [Text.unlines (reverse acc)]
      wrapWords (w:ws) acc = 
        let currentLine = case acc of
              [] -> w
              (l:_) -> l <> " " <> w
        if Text.length currentLine <= width
          then wrapWords ws (currentLine : drop 1 acc)
          else case acc of
            [] -> w : wrapWords ws []
            _ -> Text.unlines (reverse acc) : wrapWords (w:ws) []
  in Text.unlines $ wrapWords words []

indentText :: Int -> Text -> Text
indentText spaces text = 
  let indentation = Text.pack $ replicate spaces ' '
      lines' = Text.lines text
      indentedLines = map (indentation <>) lines'
  in Text.unlines indentedLines

normalizeSpaces :: Text -> Text
normalizeSpaces text = 
  let words' = Text.words text
  in Text.unwords words'

isBlank :: Text -> Bool
isBlank text = Text.all isSpace text

nonEmpty :: Text -> Bool
nonEmpty text = not $ Text.null text

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

splitOnFirst :: Text -> Text -> (Text, Text)
splitOnFirst sep text = 
  case Text.breakOn sep text of
    (before, after) -> 
      if Text.null after
        then (before, "")
        else (before, Text.drop (Text.length sep) after)

splitOnLast :: Text -> Text -> (Text, Text)
splitOnLast sep text = 
  let parts = splitOn sep text
  in case parts of
    [] -> ("", "")
    [x] -> (x, "")
    xs -> (Text.unlines $ init xs, last xs)

startsWith :: Text -> Text -> Bool
startsWith prefix text = 
  let prefixStr = Text.unpack prefix
      textStr = Text.unpack text
  in prefixStr `isPrefixOf` textStr

endsWith :: Text -> Text -> Bool
endsWith suffix text = 
  let suffixStr = Text.unpack suffix
      textStr = Text.unpack text
  in suffixStr `isSuffixOf` textStr

contains :: Text -> Text -> Bool
contains sub text = 
  let subStr = Text.unpack sub
      textStr = Text.unpack text
  in subStr `List.isInfixOf` textStr

replaceAll :: Text -> Text -> Text -> Text
replaceAll old new text = 
  let oldStr = Text.unpack old
      newStr = Text.unpack new
      textStr = Text.unpack text
  in Text.pack $ List.intercalate newStr $ List.splitOn oldStr textStr

countOccurrences :: Text -> Text -> Int
countOccurrences sub text = 
  let subStr = Text.unpack sub
      textStr = Text.unpack text
  in length $ List.splitOn subStr textStr - 1

isNumeric :: Text -> Bool
isNumeric text = 
  let textStr = Text.unpack text
  in all (`elem` "0123456789") textStr

isAlpha :: Text -> Bool
isAlpha text = 
  let textStr = Text.unpack text
  in all (`elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") textStr

isAlphaNumeric :: Text -> Bool
isAlphaNumeric text = 
  let textStr = Text.unpack text
  in all (`elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789") textStr

toLower :: Text -> Text
toLower = Text.map toLower

toUpper :: Text -> Text
toUpper = Text.map toUpper

trim :: Text -> Text
trim = Text.dropWhile isSpace . Text.dropWhileEnd isSpace

trimLeft :: Text -> Text
trimLeft = Text.dropWhile isSpace

trimRight :: Text -> Text
trimRight = Text.dropWhileEnd isSpace

lines :: Text -> [Text]
lines = Text.splitOn "\n"

unlines :: [Text] -> Text
unlines = Text.intercalate "\n"

words :: Text -> [Text]
words = Text.splitOn " "

unwords :: [Text] -> Text
unwords = Text.intercalate " "

reverse :: Text -> Text
reverse = Text.reverse

take :: Int -> Text -> Text
take = Text.take

drop :: Int -> Text -> Text
drop = Text.drop

length :: Text -> Int
length = Text.length

null :: Text -> Bool
null = Text.null

empty :: Text
empty = Text.empty

singleton :: Char -> Text
singleton = Text.singleton

cons :: Char -> Text -> Text
cons = Text.cons

snoc :: Text -> Char -> Text
snoc = Text.snoc

append :: Text -> Text -> Text
append = Text.append

concat :: [Text] -> Text
concat = Text.concat

intercalate :: Text -> [Text] -> Text
intercalate = Text.intercalate

any :: (Char -> Bool) -> Text -> Bool
any = Text.any

all :: (Char -> Bool) -> Text -> Bool
all = Text.all

filter :: (Char -> Bool) -> Text -> Text
filter = Text.filter

map :: (Char -> Char) -> Text -> Text
map = Text.map

foldl :: (a -> Char -> a) -> a -> Text -> a
foldl = Text.foldl'

foldr :: (Char -> a -> a) -> a -> Text -> a
foldr = Text.foldr
