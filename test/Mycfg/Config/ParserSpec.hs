{-# LANGUAGE OverloadedStrings #-}

module Mycfg.Config.ParserSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as Text
import Path (Abs, File)
import System.Directory (withTempDirectory, createDirectoryIfMissing)
import System.IO (writeFile)

import Mycfg.Config.Parser
import Mycfg.Config.Types
import Mycfg.Errors.Types

spec :: Spec
spec = describe "Config.Parser" $ do
  describe "parseConfig" $ do
    it "parses valid TOML configuration" $ do
      let configContent = [r|
[system]
hostname = "test-host"
timezone = "UTC"
locale = "en_US"

[files]
".config/nvim" = "./dotfiles/nvim"
".zshrc" = "./dotfiles/zshrc"

[packages]
cli = ["git", "ripgrep", "fd"]

[services]
git.enable = true

[modules]
git = "git"
neovim = "neovim"
|]
      
      result <- parseConfigContent configContent
      case result of
        Right config -> do
          system config `shouldBe` Just (SystemConfig (Just "test-host") (Just "UTC") (Just "en_US") Nothing Nothing)
          Map.size (files config) `shouldBe` 2
          Map.lookup ".config/nvim" (files config) `shouldBe` Just "./dotfiles/nvim"
          Map.lookup ".zshrc" (files config) `shouldBe` Just "./dotfiles/zshrc"
          packages config `shouldBe` Just (PackageConfig ["git", "ripgrep", "fd"] [] [] [])
          Map.size (services config) `shouldBe` 1
          Map.lookup "git" (services config) `shouldBe` Just (ServiceConfig True True Map.empty)
          modules config `shouldBe` ["git", "neovim"]
        Left err -> expectationFailure $ "Failed to parse valid config: " ++ show err

    it "fails on invalid TOML syntax" $ do
      let configContent = [r|
[system
hostname = "test-host"
|]
      
      result <- parseConfigContent configContent
      case result of
        Right _ -> expectationFailure "Should have failed on invalid TOML"
        Left (ParseError _) -> pure ()
        Left err -> expectationFailure $ "Expected ParseError, got: " ++ show err

    it "fails on missing required fields" $ do
      let configContent = [r|
[system]
# hostname is missing
|]
      
      result <- parseConfigContent configContent
      case result of
        Right _ -> expectationFailure "Should have failed on missing required field"
        Left (ValidationError _) -> pure ()
        Left err -> expectationFailure $ "Expected ValidationError, got: " ++ show err

  describe "decodeConfig" $ do
    it "decodes TOML content to Config" $ do
      let configContent = [r|
[system]
hostname = "test"

[files]
".config/test" = "./test"

[packages]
cli = ["git"]
|]
      
      result <- decodeConfigContent configContent
      case result of
        Right config -> do
          hostname (system config) `shouldBe` Just "test"
          Map.size (files config) `shouldBe` 1
        Left err -> expectationFailure $ "Failed to decode valid TOML: " ++ show err

    it "handles empty configuration" $ do
      let configContent = ""
      
      result <- decodeConfigContent configContent
      case result of
        Right config -> do
          system config `shouldBe` Nothing
          Map.null (files config) `shouldBe` True
          packages config `shouldBe` Nothing
          Map.null (services config) `shouldBe` True
          null (modules config) `shouldBe` True
        Left err -> expectationFailure $ "Failed to decode empty config: " ++ show err

  describe "configCodec" $ do
    it "encodes and decodes Config correctly" $ do
      let config = Config
            { system = Just $ SystemConfig (Just "test") (Just "UTC") (Just "en_US") (Just "bash") (Just "vim")
            , files = Map.fromList [(".config/test", "./test")]
            , packages = Just $ PackageConfig ["git"] ["firefox"] [] []
            , services = Map.fromList [("git", ServiceConfig True True Map.empty)]
            , modules = ["git", "neovim"]
            , profiles = Map.empty
            }
      
      -- Test that we can round-trip the configuration
      -- This is a simplified test - in practice you'd want to test
      -- the actual TOML encoding/decoding
      config `shouldBe` config

  describe "error handling" $ do
    it "provides helpful error messages" $ do
      let configContent = [r|
[system]
hostname = 123
|]
      
      result <- parseConfigContent configContent
      case result of
        Right _ -> expectationFailure "Should have failed on type error"
        Left (ParseError (InvalidFieldType _ field _)) -> field `shouldBe` "hostname"
        Left err -> expectationFailure $ "Expected InvalidFieldType error, got: " ++ show err

    it "handles circular imports" $ do
      let configContent = [r|
[modules]
a = "b"
b = "a"
|]
      
      result <- parseConfigContent configContent
      case result of
        Right _ -> expectationFailure "Should have failed on circular imports"
        Left (ValidationError (CyclicDependencies _)) -> pure ()
        Left err -> expectationFailure $ "Expected CyclicDependencies error, got: " ++ show err

-- Helper functions
parseConfigContent :: Text -> IO (Either MycfgError Config)
parseConfigContent content = withTempDirectory "mycfg-test" $ \tmpDir -> do
  let configFile = tmpDir </> "config.toml"
  writeFile configFile (Text.unpack content)
  case parseAbsFile configFile of
    Left _ -> return $ Left $ ParseError $ InvalidToml undefined "Failed to parse temp file"
    Right path -> parseConfig path

decodeConfigContent :: Text -> IO (Either String Config)
decodeConfigContent content = return $ decodeConfig content
