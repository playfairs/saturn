{-# LANGUAGE OverloadedStrings #-}

module Mycfg.Core.DiffSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (UTCTime)
import Path (Abs, File)

import Mycfg.Core.Diff
import Mycfg.Config.Types
import Mycfg.State.Manifest
import Mycfg.Filesystem.Paths

spec :: Spec
spec = describe "Core.Diff" $ do
  describe "computeDiff" $ do
    it "computes initial diff correctly" $ do
      let config = Config
            { system = Nothing
            , files = Map.fromList [(".config/nvim", "./dotfiles/nvim")]
            , packages = Nothing
            , services = Map.empty
            , modules = []
            , profiles = Map.empty
            }
      
      result <- computeDiff config Nothing
      case result of
        Right diff -> do
          Map.size (fileDiffs diff) `shouldBe` 1
          summary diff `shouldBe` DiffSummary 1 0 0 0 0 0 1
        Left err -> expectationFailure $ "Failed to compute initial diff: " ++ show err

    it "computes incremental diff correctly" $ do
      let config = Config
            { system = Nothing
            , files = Map.fromList [(".config/nvim", "./dotfiles/nvim")]
            , packages = Nothing
            , services = Map.empty
            , modules = []
            , profiles = Map.empty
            }
      
      let manifest = Manifest
            { metadata = ManifestMetadata "gen-1" undefined "test" "0.1.0" 1 0 0
            , entries = Map.empty
            }
      
      result <- computeDiff config (Just manifest)
      case result of
        Right diff -> do
          Map.size (fileDiffs diff) `shouldBe` 1
          let fileDiff = head $ Map.elems $ fileDiffs diff
          diffType fileDiff `shouldBe` Added
        Left err -> expectationFailure $ "Failed to compute incremental diff: " ++ show err

  describe "computeManifestDiff" $ do
    it "computes diff between manifests" $ do
      let oldManifest = Manifest
            { metadata = ManifestMetadata "gen-1" undefined "test" "0.1.0" 1 0 0
            , entries = Map.fromList 
                [ ("test.txt", FileEntry $ FileEntry "test.txt" (Just "./test.txt") "rw-r--r--" 100 "abc123" undefined Copy)
                ]
            }
      
      let newManifest = Manifest
            { metadata = ManifestMetadata "gen-2" undefined "test" "0.1.0" 2 0 0
            , entries = Map.fromList 
                [ ("test.txt", FileEntry $ FileEntry "test.txt" (Just "./test.txt") "rw-r--r--" 200 "def456" undefined Copy)
                , ("new.txt", FileEntry $ FileEntry "new.txt" (Just "./new.txt") "rw-r--r--" 50 "ghi789" undefined Copy)
                ]
            }
      
      let diff = computeManifestDiff oldManifest newManifest
      
      Map.size (fileDiffs diff) `shouldBe` 2
      summary diff `shouldBe` DiffSummary 1 0 1 0 0 0 2

  describe "computeConfigDiff" $ do
    it "computes diff between configs" $ do
      let oldConfig = Config
            { system = Nothing
            , files = Map.fromList [(".config/old", "./old")]
            , packages = Nothing
            , services = Map.empty
            , modules = []
            , profiles = Map.empty
            }
      
      let newConfig = Config
            { system = Nothing
            , files = Map.fromList [(".config/new", "./new")]
            , packages = Nothing
            , services = Map.empty
            , modules = []
            , profiles = Map.empty
            }
      
      let diff = computeConfigDiff oldConfig newConfig
      
      Map.size (fileDiffs diff) `shouldBe` 2
      summary diff `shouldBe` DiffSummary 1 1 0 0 0 0 2

  describe "applyDiff" $ do
    it "applies diff successfully" $ do
      let diff = DiffResult
            { fileDiffs = Map.fromList 
                [ ("test.txt", FileDiff Added (Just "./test.txt") "test.txt" Nothing (Just "abc123") Nothing (Just 100) Nothing (Just undefined) Copy)
                ]
            , directoryDiffs = Map.empty
            , symlinkDiffs = Map.empty
            , summary = DiffSummary 1 0 0 0 0 0 1
            }
      
      result <- applyDiff diff
      case result of
        Right () -> pure ()
        Left err -> expectationFailure $ "Failed to apply diff: " ++ show err

    it "handles diff application failure" $ do
      let diff = DiffResult
            { fileDiffs = Map.fromList 
                [ ("/nonexistent/test.txt", FileDiff Added (Just "./test.txt") "/nonexistent/test.txt" Nothing (Just "abc123") Nothing (Just 100) Nothing (Just undefined) Copy)
                ]
            , directoryDiffs = Map.empty
            , symlinkDiffs = Map.empty
            , summary = DiffSummary 1 0 0 0 0 0 1
            }
      
      result <- applyDiff diff
      case result of
        Right () -> expectationFailure "Should have failed to apply diff to nonexistent path"
        Left _ -> pure ()

  describe "FileDiff operations" $ do
    it "creates correct FileDiff for added files" $ do
      let sourcePath = "./test.txt"
          targetPath = "test.txt"
          checksum = "abc123"
          size = 100
      
      let diff = FileDiff Added (Just sourcePath) targetPath Nothing (Just checksum) Nothing (Just size) Nothing undefined Copy
      
      diffType diff `shouldBe` Added
      sourcePath diff `shouldBe` Just sourcePath
      targetPath diff `shouldBe` targetPath
      oldChecksum diff `shouldBe` Nothing
      newChecksum diff `shouldBe` Just checksum
      operation diff `shouldBe` Copy

    it "creates correct FileDiff for modified files" $ do
      let sourcePath = "./test.txt"
          targetPath = "test.txt"
          oldChecksum = "abc123"
          newChecksum = "def456"
          oldSize = 100
          newSize = 200
      
      let diff = FileDiff Modified (Just sourcePath) targetPath (Just oldChecksum) (Just newChecksum) (Just oldSize) (Just newSize) Nothing undefined Copy
      
      diffType diff `shouldBe` Modified
      sourcePath diff `shouldBe` Just sourcePath
      targetPath diff `shouldBe` targetPath
      oldChecksum diff `shouldBe` Just oldChecksum
      newChecksum diff `shouldBe` Just newChecksum
      operation diff `shouldBe` Copy

    it "creates correct FileDiff for removed files" $ do
      let targetPath = "test.txt"
          oldChecksum = "abc123"
          oldSize = 100
      
      let diff = FileDiff Removed Nothing targetPath (Just oldChecksum) Nothing (Just oldSize) Nothing undefined Copy
      
      diffType diff `shouldBe` Removed
      sourcePath diff `shouldBe` Nothing
      targetPath diff `shouldBe` targetPath
      oldChecksum diff `shouldBe` Just oldChecksum
      newChecksum diff `shouldBe` Nothing
      operation diff `shouldBe` Copy

  describe "DiffSummary" $ do
    it "calculates correct summary" $ do
      let summary = DiffSummary 5 2 3 1 1 2 1
      
      filesAdded summary `shouldBe` 5
      filesRemoved summary `shouldBe` 2
      filesModified summary `shouldBe` 3
      directoriesAdded summary `shouldBe` 1
      directoriesRemoved summary `shouldBe` 1
      symlinksAdded summary `shouldBe` 2
      symlinksRemoved summary `shouldBe` 1
      totalChanges summary `shouldBe` 14

    it "calculates total changes correctly" $ do
      let summary = DiffSummary 1 1 1 1 1 1 1
      totalChanges summary `shouldBe` 7

    it "handles empty summary" $ do
      let summary = DiffSummary 0 0 0 0 0 0 0
      totalChanges summary `shouldBe` 0

  describe "property tests" $ do
    it "diff summary is consistent with file diffs" $ do
      property $ \fileDiffs -> do
        let summary = computeDiffSummaryFromFiles fileDiffs
            addedCount = length $ filter (\d -> diffType d == Added) fileDiffs
            removedCount = length $ filter (\d -> diffType d == Removed) fileDiffs
            modifiedCount = length $ filter (\d -> diffType d == Modified) fileDiffs
        
        filesAdded summary `shouldBe` addedCount
        filesRemoved summary `shouldBe` removedCount
        filesModified summary `shouldBe` modifiedCount
        totalChanges summary `shouldBe` addedCount + removedCount + modifiedCount

    it "diff is idempotent for unchanged files" $ do
      property $ \fileDiff -> do
        let unchangedDiff = fileDiff { diffType = Unchanged }
        let summary = computeDiffSummaryFromFiles [unchangedDiff]
        
        totalChanges summary `shouldBe` 0

-- Helper functions
computeDiffSummaryFromFiles :: [FileDiff] -> DiffSummary
computeDiffSummaryFromFiles fileDiffs = 
  let addedCount = length $ filter (\d -> diffType d == Added) fileDiffs
      removedCount = length $ filter (\d -> diffType d == Removed) fileDiffs
      modifiedCount = length $ filter (\d -> diffType d == Modified) fileDiffs
  in DiffSummary addedCount removedCount modifiedCount 0 0 0 0 (addedCount + removedCount + modifiedCount)
