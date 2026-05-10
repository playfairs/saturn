{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import qualified Mycfg.Config.ParserSpec
import qualified Mycfg.Config.ValidatorSpec
import qualified Mycfg.Core.DiffSpec
import qualified Mycfg.Core.PlannerSpec
import qualified Mycfg.Core.ApplySpec
import qualified Mycfg.Filesystem.AtomicSpec
import qualified Mycfg.Filesystem.SymlinkSpec
import qualified Mycfg.Filesystem.CopySpec
import qualified Mycfg.Filesystem.PermissionsSpec
import qualified Mycfg.Modules.LoaderSpec
import qualified Mycfg.Modules.RegistrySpec
import qualified Mycfg.Modules.ResolverSpec
import qualified Mycfg.State.StoreSpec
import qualified Mycfg.State.ManifestSpec
import qualified Mycfg.State.SnapshotSpec
import qualified Mycfg.State.GenerationsSpec
import qualified Mycfg.Utils.TextSpec
import qualified Mycfg.Utils.HashSpec
import qualified Mycfg.Utils.TimeSpec
import qualified Mycfg.Utils.ProcessSpec

main :: IO ()
main = hspec $ do
  describe "Mycfg.Config.Parser" Mycfg.Config.ParserSpec.spec
  describe "Mycfg.Config.Validator" Mycfg.Config.ValidatorSpec.spec
  describe "Mycfg.Core.Diff" Mycfg.Core.DiffSpec.spec
  describe "Mycfg.Core.Planner" Mycfg.Core.PlannerSpec.spec
  describe "Mycfg.Core.Apply" Mycfg.Core.ApplySpec.spec
  describe "Mycfg.Filesystem.Atomic" Mycfg.Filesystem.AtomicSpec.spec
  describe "Mycfg.Filesystem.Symlink" Mycfg.Filesystem.SymlinkSpec.spec
  describe "Mycfg.Filesystem.Copy" Mycfg.Filesystem.CopySpec.spec
  describe "Mycfg.Filesystem.Permissions" Mycfg.Filesystem.PermissionsSpec.spec
  describe "Mycfg.Modules.Loader" Mycfg.Modules.LoaderSpec.spec
  describe "Mycfg.Modules.Registry" Mycfg.Modules.RegistrySpec.spec
  describe "Mycfg.Modules.Resolver" Mycfg.Modules.ResolverSpec.spec
  describe "Mycfg.State.Store" Mycfg.State.StoreSpec.spec
  describe "Mycfg.State.Manifest" Mycfg.State.ManifestSpec.spec
  describe "Mycfg.State.Snapshot" Mycfg.State.SnapshotSpec.spec
  describe "Mycfg.State.Generations" Mycfg.State.GenerationsSpec.spec
  describe "Mycfg.Utils.Text" Mycfg.Utils.TextSpec.spec
  describe "Mycfg.Utils.Hash" Mycfg.Utils.HashSpec.spec
  describe "Mycfg.Utils.Time" Mycfg.Utils.TimeSpec.spec
  describe "Mycfg.Utils.Process" Mycfg.Utils.ProcessSpec.spec
