{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mycfg.Config.Schema (
    ConfigSchema (..),
    FieldDefinition (..),
    FieldType (..),
    RingSchema (..),
    RingInput (..),
    RingModule (..),
    RingProfile (..),
    validateSchema,
    getDefaultSchema,
    defaultRingSchema,
) where

import qualified Data.Map as Map
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

import Mycfg.Config.Types

data ConfigSchema = ConfigSchema
    { fields :: Map Text FieldDefinition
    , requiredFields :: Set Text
    , optionalFields :: Set Text
    }
    deriving (Show, Eq)

data FieldDefinition = FieldDefinition
    { fieldType :: FieldType
    , description :: Text
    , defaultValue :: Maybe Text
    , validationRules :: [ValidationRule]
    }
    deriving (Show, Eq)

data FieldType
    = StringField
    | NumberField
    | BooleanField
    | ArrayField FieldType
    | ObjectField ConfigSchema
    | MapField FieldType
    | EnumField [Text]
    deriving (Show, Eq)

data ValidationRule
    = MinLength Int
    | MaxLength Int
    | Pattern Text
    | Required
    | Optional
    | OneOf [Text]
    deriving (Show, Eq)

data RingSchema = RingSchema
    { ringName :: Text
    , ringDescription :: Maybe Text
    , ringInputs :: [RingInput]
    , ringModules :: [RingModule]
    , ringProfiles :: [RingProfile]
    }
    deriving (Show, Eq, Generic)

instance ToJSON RingSchema
instance FromJSON RingSchema

data RingInput = RingInput
    { inputName :: Text
    , inputUrl :: Text
    , inputType :: Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON RingInput
instance FromJSON RingInput

data RingModule = RingModule
    { modulePath :: Text
    , moduleEnabled :: Bool
    }
    deriving (Show, Eq, Generic)

instance ToJSON RingModule
instance FromJSON RingModule

data RingProfile = RingProfile
    { profileName :: Text
    , profileModules :: [Text]
    }
    deriving (Show, Eq, Generic)

instance ToJSON RingProfile
instance FromJSON RingProfile

validateSchema :: Config -> ConfigSchema -> Either Text ()
validateSchema config schema = Right ()

defaultRingSchema :: RingSchema
defaultRingSchema =
    RingSchema
        { ringName = "default"
        , ringDescription = Just "Default Saturn Ring"
        , ringInputs = []
        , ringModules = []
        , ringProfiles = []
        }

getDefaultSchema :: ConfigSchema
getDefaultSchema =
    ConfigSchema
        { fields =
            Map.fromList
                [ ("system", FieldDefinition ObjectField systemSchema "System configuration" Nothing [])
                , ("files", FieldDefinition (MapField StringField) "File mappings" Nothing [])
                , ("packages", FieldDefinition ObjectField packageSchema "Package configuration" Nothing [])
                , ("services", FieldDefinition (MapField (ObjectField serviceSchema)) "Service configuration" Nothing [])
                , ("modules", FieldDefinition (ArrayField StringField) "Module list" Nothing [])
                , ("profiles", FieldDefinition (MapField (ObjectField profileSchema)) "Profile configurations" Nothing [])
                ]
        , requiredFields = Set.fromList []
        , optionalFields = Set.fromList ["system", "packages", "services", "modules", "profiles"]
        }

systemSchema :: ConfigSchema
systemSchema =
    ConfigSchema
        { fields =
            Map.fromList
                [ ("hostname", FieldDefinition StringField "System hostname" Nothing [MaxLength 253])
                , ("timezone", FieldDefinition StringField "System timezone" Nothing [Pattern "^[A-Za-z_]+/[A-Za-z_]+$"])
                , ("locale", FieldDefinition StringField "System locale" Nothing [Pattern "^[a-z][a-z]_[A-Z][A-Z]$"])
                , ("shell", FieldDefinition StringField "Default shell" Nothing [OneOf ["bash", "zsh", "fish", "nu"]])
                , ("editor", FieldDefinition StringField "Default editor" Nothing [])
                ]
        , requiredFields = Set.fromList []
        , optionalFields = Set.fromList ["hostname", "timezone", "locale", "shell", "editor"]
        }

packageSchema :: ConfigSchema
packageSchema =
    ConfigSchema
        { fields =
            Map.fromList
                [ ("cli", FieldDefinition (ArrayField StringField) "CLI packages" Nothing [])
                , ("gui", FieldDefinition (ArrayField StringField) "GUI packages" Nothing [])
                , ("development", FieldDefinition (ArrayField StringField) "Development packages" Nothing [])
                , ("system", FieldDefinition (ArrayField StringField) "System packages" Nothing [])
                ]
        , requiredFields = Set.fromList []
        , optionalFields = Set.fromList ["cli", "gui", "development", "system"]
        }

serviceSchema :: ConfigSchema
serviceSchema =
    ConfigSchema
        { fields =
            Map.fromList
                [ ("enable", FieldDefinition BooleanField "Enable service" Nothing [])
                , ("start", FieldDefinition BooleanField "Start service" Nothing [])
                , ("config", FieldDefinition (MapField StringField) "Service configuration" Nothing [])
                ]
        , requiredFields = Set.fromList ["enable", "start"]
        , optionalFields = Set.fromList ["config"]
        }

profileSchema :: ConfigSchema
profileSchema =
    ConfigSchema
        { fields =
            Map.fromList
                [ ("name", FieldDefinition StringField "Profile name" Nothing [Required, MinLength 1])
                , ("description", FieldDefinition StringField "Profile description" Nothing [Required, MinLength 1])
                , ("modules", FieldDefinition (ArrayField StringField) "Profile modules" Nothing [Required])
                , ("extends", FieldDefinition (ArrayField StringField) "Parent profiles" Nothing [])
                ]
        , requiredFields = Set.fromList ["name", "description", "modules"]
        , optionalFields = Set.fromList ["extends"]
        }
