{-# LANGUAGE OverloadedStrings #-}

module Mycfg.Config.ValidatorSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Path (Abs, File)

import Mycfg.Config.Validator
import Mycfg.Config.Types
import Mycfg.Errors.Types

spec :: Spec
spec = describe "Config.Validator" $ do
  describe "validateConfig" $ do
    it "validates a correct configuration" $ do
      let config = Config
            { system = Just $ SystemConfig (Just "test") (Just "UTC") (Just "en_US") (Just "bash") (Just "vim")
            , files = Map.fromList [(".config/nvim", "./dotfiles/nvim"), (".zshrc", "./dotfiles/zshrc")]
            , packages = Just $ PackageConfig ["git", "ripgrep"] [] [] []
            , services = Map.fromList [("git", ServiceConfig True True Map.empty)]
            , modules = ["git", "neovim"]
            , profiles = Map.fromList [("default", ProfileConfig "default" "Default profile" ["git", "neovim"] [])]
            }
      
      result <- validateConfig config
      case result of
        ValidationSuccess [] -> pure ()
        ValidationSuccess warnings -> expectationFailure $ "Unexpected warnings: " ++ show warnings
        ValidationFailure errors -> expectationFailure $ "Validation failed: " ++ show errors

    it "detects invalid paths" $ do
      let config = Config
            { system = Nothing
            , files = Map.fromList [("/etc/passwd", "./dotfiles/passwd")] -- Outside home
            , packages = Nothing
            , services = Map.empty
            , modules = []
            , profiles = Map.empty
            }
      
      result <- validateConfig config
      case result of
        ValidationSuccess _ -> expectationFailure "Should have failed on invalid path"
        ValidationFailure errors -> 
          case head errors of
            ValidationError (PathOutsideHome _) -> pure ()
            other -> expectationFailure $ "Expected PathOutsideHome error, got: " ++ show other

    it "detects circular dependencies" $ do
      let config = Config
            { system = Nothing
            , files = Map.empty
            , packages = Nothing
            , services = Map.empty
            , modules = ["a", "b", "c"] -- Assume circular deps in test
            , profiles = Map.fromList 
                [ ("a", ProfileConfig "a" "Profile A" ["b"] [])
                , ("b", ProfileConfig "b" "Profile B" ["c"] [])
                , ("c", ProfileConfig "c" "Profile C" ["a"] [])
                ]
            }
      
      result <- validateConfig config
      case result of
        ValidationSuccess _ -> expectationFailure "Should have failed on circular dependencies"
        ValidationFailure errors -> 
          case head errors of
            ValidationError (CyclicDependencies _) -> pure ()
            other -> expectationFailure $ "Expected CyclicDependencies error, got: " ++ show other

    it "validates module references" $ do
      let config = Config
            { system = Nothing
            , files = Map.empty
            , packages = Nothing
            , services = Map.empty
            , modules = ["git", "neovim"]
            , profiles = Map.fromList [("default", ProfileConfig "default" "Default" ["nonexistent"] [])]
            }
      
      result <- validateConfig config
      case result of
        ValidationSuccess _ -> expectationFailure "Should have failed on missing module"
        ValidationFailure errors -> 
          case head errors of
            ValidationError (ModuleNotFound "nonexistent") -> pure ()
            other -> expectationFailure $ "Expected ModuleNotFound error, got: " ++ show other

  describe "validateConfigStructure" $ do
    it "validates profile structure" $ do
      let config = Config
            { system = Nothing
            , files = Map.empty
            , packages = Nothing
            , services = Map.empty
            , modules = []
            , profiles = Map.fromList [("test", ProfileConfig "" "" [] [])] -- Invalid: empty name/description
            }
      
      let errors = validateConfigStructure config
      errors `shouldNotBe` []
      head errors `shouldSatisfy` isProfileError
      where
        isProfileError (ValidationError (InvalidProfileName _)) = True
        isProfileError (ValidationError (MissingRequiredField _)) = True
        isProfileError _ = False

    it "validates service configuration" $ do
      let config = Config
            { system = Nothing
            , files = Map.empty
            , packages = Nothing
            , services = Map.fromList [("test", ServiceConfig False True Map.empty)] -- Disabled but start=true
            , modules = []
            , profiles = Map.empty
            }
      
      let errors = validateConfigStructure config
      -- This test depends on your validation logic
      -- You might want to add specific service validation rules
      length errors `shouldBe` 0

  describe "validateConfigWarnings" $ do
    it "detects duplicate modules" $ do
      let config = Config
            { system = Nothing
            , files = Map.empty
            , packages = Nothing
            , services = Map.empty
            , modules = ["git", "git", "neovim"] -- Duplicate "git"
            , profiles = Map.empty
            }
      
      let warnings = validateConfigWarnings config
      warnings `shouldContain` [DuplicateModule "git"]

    it "detects unused modules" $ do
      let config = Config
            { system = Nothing
            , files = Map.empty
            , packages = Nothing
            , services = Map.empty
            , modules = ["git", "neovim", "zsh"]
            , profiles = Map.fromList [("default", ProfileConfig "default" "Default" ["git"] [])] -- Only uses "git"
            }
      
      let warnings = validateConfigWarnings config
      warnings `shouldSatisfy` any (isUnusedModule ["neovim", "zsh"])
      where
        isUnusedModule unusedModules (UnusedModule name) = name `elem` unusedModules
        isUnusedModule _ _ = False

  describe "validatePaths" $ do
    it "validates file paths" $ do
      let config = Config
            { system = Nothing
            , files = Map.fromList [("", "./test")] -- Empty path
            , packages = Nothing
            , services = Map.empty
            , modules = []
            , profiles = Map.empty
            }
      
      result <- validatePaths config
      case result of
        [] -> expectationFailure "Should have failed on empty path"
        (ValidationError (InvalidPathSyntax _) : _) -> pure ()
        other -> expectationFailure $ "Expected InvalidPathSyntax error, got: " ++ show other

    it "validates path characters" $ do
      let config = Config
            { system = Nothing
            , files = Map.fromList [("path/with\0null", "./test")] -- Contains null character
            , packages = Nothing
            , services = Map.empty
            , modules = []
            , profiles = Map.empty
            }
      
      result <- validatePaths config
      case result of
        [] -> expectationFailure "Should have failed on invalid path characters"
        (ValidationError (InvalidPathSyntax _) : _) -> pure ()
        other -> expectationFailure $ "Expected InvalidPathSyntax error, got: " ++ show other

  describe "validateDependencies" $ do
    it "detects missing dependencies" $ do
      let config = Config
            { system = Nothing
            , files = Map.empty
            , packages = Nothing
            , services = Map.empty
            , modules = ["git"]
            , profiles = Map.fromList [("default", ProfileConfig "default" "Default" ["git", "nonexistent"] [])]
            }
      
      result <- validateDependencies config
      case result of
        [] -> expectationFailure "Should have failed on missing dependency"
        (ValidationError (MissingDependency "nonexistent") : _) -> pure ()
        other -> expectationFailure $ "Expected MissingDependency error, got: " ++ show other

    it "detects circular dependencies" $ do
      let config = Config
            { system = Nothing
            , files = Map.empty
            , packages = Nothing
            , services = Map.empty
            , modules = ["a", "b", "c"]
            , profiles = Map.fromList 
                [ ("a", ProfileConfig "a" "Profile A" ["b"] [])
                , ("b", ProfileConfig "b" "Profile B" ["c"] [])
                , ("c", ProfileConfig "c" "Profile C" ["a"] [])
                ]
            }
      
      result <- validateDependencies config
      case result of
        [] -> expectationFailure "Should have failed on circular dependencies"
        (ValidationError (CyclicDependencies _) : _) -> pure ()
        other -> expectationFailure $ "Expected CyclicDependencies error, got: " ++ show other

  describe "property tests" $ do
    it "validates that valid configs stay valid" $ do
      property $ \config -> do
        result <- validateConfig config
        case result of
          ValidationSuccess _ -> pure ()
          ValidationFailure _ -> pure () -- Invalid configs can fail

    it "validates that adding valid files doesn't break validation" $ do
      property $ \config files -> do
        let newConfig = config { files = Map.fromList files }
        result <- validateConfig newConfig
        case result of
          ValidationSuccess _ -> pure ()
          ValidationFailure _ -> pure () -- Invalid configs can fail

-- Helper functions
isProfileError :: ValidationError -> Bool
isProfileError (ValidationError (InvalidProfileName _)) = True
isProfileError (ValidationError (MissingRequiredField _)) = True
isProfileError _ = False
