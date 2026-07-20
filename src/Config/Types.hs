{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mycfg.Config.Types (
    Config (..),
    SystemConfig (..),
    FileConfig (..),
    PackageConfig (..),
    ServiceConfig (..),
    ModuleConfig (..),
    ProfileConfig (..),
    FileOperation (..),
    ServiceState (..),
    ConfigPath,
    ModuleName,
    ProfileName,
    TargetPath,
    SourcePath,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

import Mycfg.Errors.Types

type ConfigPath = Path Abs File
type ModuleName = Text
type ProfileName = Text
type TargetPath = Text
type SourcePath = Text

data Config = Config
    { system :: Maybe SystemConfig
    , files :: Map TargetPath SourcePath
    , packages :: Maybe PackageConfig
    , services :: Map Text ServiceConfig
    , modules :: [ModuleName]
    , profiles :: Map ProfileName ProfileConfig
    }
    deriving (Show, Eq, Generic)

instance ToJSON Config
instance FromJSON Config

data SystemConfig = SystemConfig
    { hostname :: Maybe Text
    , timezone :: Maybe Text
    , locale :: Maybe Text
    , shell :: Maybe Text
    , editor :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON SystemConfig
instance FromJSON SystemConfig

data FileConfig = FileConfig
    { operation :: FileOperation
    , source :: SourcePath
    , target :: TargetPath
    , permissions :: Maybe Text
    , owner :: Maybe Text
    , group :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON FileConfig
instance FromJSON FileConfig

data PackageConfig = PackageConfig
    { cli :: [Text]
    , gui :: [Text]
    , development :: [Text]
    , system :: [Text]
    }
    deriving (Show, Eq, Generic)

instance ToJSON PackageConfig
instance FromJSON PackageConfig

data ServiceConfig = ServiceConfig
    { enable :: Bool
    , start :: Bool
    , config :: Map Text Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON ServiceConfig
instance FromJSON ServiceConfig

data ModuleConfig = ModuleConfig
    { name :: ModuleName
    , description :: Text
    , version :: Text
    , dependencies :: [ModuleName]
    , config :: Map Text Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON ModuleConfig
instance FromJSON ModuleConfig

data ProfileConfig = ProfileConfig
    { name :: ProfileName
    , description :: Text
    , modules :: [ModuleName]
    , extends :: [ProfileName]
    }
    deriving (Show, Eq, Generic)

instance ToJSON ProfileConfig
instance FromJSON ProfileConfig

data FileOperation
    = Symlink
    | Copy
    | Hardlink
    | Template
    deriving (Show, Eq, Generic)

instance ToJSON FileOperation
instance FromJSON FileOperation

data ServiceState
    = Enabled
    | Disabled
    | Running
    | Stopped
    deriving (Show, Eq, Generic)

instance ToJSON ServiceState
instance FromJSON ServiceState
