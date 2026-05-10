{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mycfg.Config.Defaults
  ( defaultConfig
  , defaultSystemConfig
  , defaultPackageConfig
  , defaultServiceConfig
  , defaultProfileConfig
  , emptyConfig
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Mycfg.Config.Types

defaultConfig :: Config
defaultConfig = Config
  { system = Just defaultSystemConfig
  , files = Map.empty
  , packages = Just defaultPackageConfig
  , services = Map.empty
  , modules = []
  , profiles = Map.fromList [("default", defaultProfileConfig)]
  }

emptyConfig :: Config
emptyConfig = Config
  { system = Nothing
  , files = Map.empty
  , packages = Nothing
  , services = Map.empty
  , modules = []
  , profiles = Map.empty
  }

defaultSystemConfig :: SystemConfig
defaultSystemConfig = SystemConfig
  { hostname = Nothing
  , timezone = Just "UTC"
  , locale = Just "en_US"
  , shell = Just "bash"
  , editor = Just "vim"
  }

defaultPackageConfig :: PackageConfig
defaultPackageConfig = PackageConfig
  { cli = ["git", "ripgrep", "fd"]
  , gui = []
  , development = []
  , system = []
  }

defaultServiceConfig :: ServiceConfig
defaultServiceConfig = ServiceConfig
  { enable = True
  , start = True
  , config = Map.empty
  }

defaultProfileConfig :: ProfileConfig
defaultProfileConfig = ProfileConfig
  { name = "default"
  , description = "Default configuration profile"
  , modules = []
  , extends = []
  }
