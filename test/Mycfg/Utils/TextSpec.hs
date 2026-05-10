{-# LANGUAGE OverloadedStrings #-}

module Mycfg.Utils.TextSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as Text

import Mycfg.Utils.Text

spec :: Spec
spec = describe "Utils.Text" $ do
  describe "stripPrefix" $ do
    it "strips prefix when present" $ do
      stripPrefix "hello" "hello world" `shouldBe` Just " world"
    
    it "returns Nothing when prefix not present" $ do
      stripPrefix "hello" "world hello" `shouldBe` Nothing
    
    it "handles empty prefix" $ do
      stripPrefix "" "hello" `shouldBe` Just "hello"
    
    it "handles empty text" $ do
      stripPrefix "hello" "" `shouldBe` Nothing

  describe "stripSuffix" $ do
    it "strips suffix when present" $ do
      stripSuffix "world" "hello world" `shouldBe` Just "hello "
    
    it "returns Nothing when suffix not present" $ do
      stripSuffix "world" "hello world!" `shouldBe` Nothing
    
    it "handles empty suffix" $ do
      stripSuffix "" "hello" `shouldBe` Just "hello"
    
    it "handles empty text" $ do
      stripSuffix "world" "" `shouldBe` Nothing

  describe "splitOn" $ do
    it "splits on delimiter" $ do
      splitOn "," "a,b,c" `shouldBe` ["a", "b", "c"]
    
    it "handles empty delimiter" $ do
      splitOn "" "abc" `shouldBe` ["abc"]
    
    it "handles empty string" $ do
      splitOn "," "" `shouldBe` [""]
    
    it "handles multiple delimiters" $ do
      splitOn "," "a,,b,c" `shouldBe` ["a", "", "b", "c"]

  describe "joinWith" $ do
    it "joins texts with delimiter" $ do
      joinWith "," ["a", "b", "c"] `shouldBe` "a,b,c"
    
    it "handles empty list" $ do
      joinWith "," [] `shouldBe` ""
    
    it "handles single element" $ do
      joinWith "," ["a"] `shouldBe` "a"

  describe "capitalize" $ do
    it "capitalizes first character" $ do
      capitalize "hello" `shouldBe` "Hello"
    
    it "handles empty string" $ do
      capitalize "" `shouldBe` ""
    
    it "handles already capitalized" $ do
      capitalize "Hello" `shouldBe` "Hello"

  describe "uncapitalize" $ do
    it "uncapitalizes first character" $ do
      uncapitalize "Hello" `shouldBe` "hello"
    
    it "handles empty string" $ do
      uncapitalize "" `shouldBe` ""
    
    it "handles already uncapitalized" $ do
      uncapitalize "hello" `shouldBe` "hello"

  describe "camelToSnake" $ do
    it "converts camelCase to snake_case" $ do
      camelToSnake "helloWorld" `shouldBe` "hello_world"
    
    it "handles single word" $ do
      camelToSnake "hello" `shouldBe` "hello"
    
    it "handles empty string" $ do
      camelToSnake "" `shouldBe` ""
    
    it "handles acronyms" $ do
      camelToSnake "HTTPServer" `shouldBe` "h_t_t_p_server"

  describe "snakeToCamel" $ do
    it "converts snake_case to camelCase" $ do
      snakeToCamel "hello_world" `shouldBe` "helloWorld"
    
    it "handles single word" $ do
      snakeToCamel "hello" `shouldBe` "hello"
    
    it "handles empty string" $ do
      snakeToCamel "" `shouldBe` ""
    
    it "handles multiple underscores" $ do
      snakeToCamel "hello_world_test" `shouldBe` "helloWorldTest"

  describe "escapeShell" $ do
    it "escapes shell special characters" $ do
      escapeShell "hello world" `shouldBe` "'hello world'"
    
    it "handles single quotes" $ do
      escapeShell "hello'world" `shouldBe` "'hello'\"'\"'world'"
    
    it "handles empty string" $ do
      escapeShell "" `shouldBe` "''"
    
    it "doesn't escape safe strings" $ do
      escapeShell "hello-world" `shouldBe` "hello-world"

  describe "unescapeShell" $ do
    it "unescapes shell escaped strings" $ do
      unescapeShell "'hello world'" `shouldBe` "hello world"
    
    it "handles single quotes" $ do
      unescapeShell "'hello'\"'\"'world'" `shouldBe` "hello'world"
    
    it "handles empty string" $ do
      unescapeShell "''" `shouldBe` ""

  describe "truncateText" $ do
    it "truncates long text" $ do
      truncateText 5 "hello world" `shouldBe` "he..."
    
    it "doesn't truncate short text" $ do
      truncateText 10 "hello" `shouldBe` "hello"
    
    it "handles exact length" $ do
      truncateText 5 "hello" `shouldBe` "hello"

  describe "padText" $ do
    it "pads text to specified width" $ do
      padText 10 "hello" `shouldBe` "hello     "
    
    it "doesn't pad if already at width" $ do
      padText 5 "hello" `shouldBe` "hello"
    
    it "truncates if longer than width" $ do
      padText 3 "hello" `shouldBe` "hel"

  describe "wrapText" $ do
    it "wraps long text" $ do
      wrapText 10 "hello world test" `shouldBe` "hello\nworld\ntest"
    
    it "handles short text" $ do
      wrapText 20 "hello world" `shouldBe` "hello world"
    
    it "handles empty string" $ do
      wrapText 10 "" `shouldBe` ""

  describe "indentText" $ do
    it "indents text" $ do
      indentText 2 "hello\nworld" `shouldBe` "  hello\n  world"
    
    it "handles empty string" $ do
      indentText 2 "" `shouldBe` ""

  describe "normalizeSpaces" $ do
    it "normalizes multiple spaces" $ do
      normalizeSpaces "hello   world" `shouldBe` "hello world"
    
    it "handles leading/trailing spaces" $ do
      normalizeSpaces "  hello world  " `shouldBe` "hello world"
    
    it "handles empty string" $ do
      normalizeSpaces "" `shouldBe` ""

  describe "isBlank" $ do
    it "detects blank strings" $ do
      isBlank "   " `shouldBe` True
      isBlank "" `shouldBe` True
      isBlank "\t\n" `shouldBe` True
    
    it "detects non-blank strings" $ do
      isBlank "hello" `shouldBe` False
      isBlank " hello " `shouldBe` False

  describe "nonEmpty" $ do
    it "detects non-empty strings" $ do
      nonEmpty "hello" `shouldBe` True
      nonEmpty " " `shouldBe` True
    
    it "detects empty strings" $ do
      nonEmpty "" `shouldBe` False

  describe "safeHead" $ do
    it "returns head of non-empty list" $ do
      safeHead [1, 2, 3] `shouldBe` Just 1
    
    it "returns Nothing for empty list" $ do
      safeHead [] `shouldBe` Nothing

  describe "safeLast" $ do
    it "returns last of non-empty list" $ do
      safeLast [1, 2, 3] `shouldBe` Just 3
    
    it "returns Nothing for empty list" $ do
      safeLast [] `shouldBe` Nothing

  describe "safeInit" $ do
    it "returns init of non-empty list" $ do
      safeInit [1, 2, 3] `shouldBe` [1, 2]
    
    it "returns empty list for empty list" $ do
      safeInit [] `shouldBe` []

  describe "safeTail" $ do
    it "returns tail of non-empty list" $ do
      safeTail [1, 2, 3] `shouldBe` [2, 3]
    
    it "returns empty list for empty list" $ do
      safeTail [] `shouldBe` []

  describe "property tests" $ do
    it "splitOn and joinWith are inverses for non-empty delimiters" $ do
      property $ \text delimiter -> 
        not (Text.null delimiter) ==> 
        let parts = splitOn delimiter text
        in joinWith delimiter parts == text

    it "capitalize and uncapitalize are inverses" $ do
      property $ \text -> 
        let capitalized = capitalize text
        in if Text.null text
          then uncapitalize capitalized == ""
          else Text.length text >= 1 ==> 
            uncapitalize (capitalize text) == Text.toLower (Text.take 1 text) `Text.append` Text.drop 1 text

    it "camelToSnake and snakeToCamel are inverses for simple cases" $ do
      property $ \text -> 
        let snake = camelToSnake text
            camel = snakeToCamel snake
        in Text.null text || Text.all (`elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_") text ==> 
          camel == text

    it "escapeShell and unescapeShell are inverses" $ do
      property $ \text -> 
        let escaped = escapeShell text
            unescaped = unescapeShell escaped
        in unescaped == text

    it "truncateText preserves length constraint" $ do
      property $ \text len -> 
        let truncated = truncateText len text
        in Text.length truncated <= max 3 len

    it "padText results in correct length" $ do
      property $ \text width -> 
        let padded = padText width text
        in Text.length padded >= min (Text.length text) width

    it "wrapText doesn't increase line length beyond limit" $ do
      property $ \text width -> 
        let wrapped = wrapText width text
            lines' = Text.lines wrapped
        in all (\line -> Text.length line <= width || width < 3) lines'

    it "indentText adds consistent indentation" $ do
      property $ \text spaces -> 
        let indented = indentText spaces text
            lines' = Text.lines indented
        in all (\line -> Text.take (spaces `mod` 10) line == Text.replicate (spaces `mod` 10) " ") lines'

    it "normalizeSpaces removes consecutive spaces" $ do
      property $ \text -> 
        let normalized = normalizeSpaces text
        in not (Text.isInfixOf "  " normalized)

    it "safeHead and safeTail reconstruct original list" $ do
      property $ \list -> 
        case safeHead list of
          Nothing -> safeTail list == []
          Just head' -> head' : safeTail list == list

    it "safeInit and safeLast reconstruct original list" $ do
      property $ \list -> 
        case safeLast list of
          Nothing -> safeInit list == []
          Just last' -> safeInit list ++ [last'] == list
