{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mycfg.Config.Parser (
    parseConfig,
    parseConfigFile,
    decodeConfig,
) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class
import Data.Aeson (eitherDecodeFileStrict)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyTextEncoding
import Path (Abs, File, Path, toFilePath)
import System.Directory (doesFileExist)
import Toml (TomlCodec, TomlError, decode, (.:), (.:?), (.=), (.=!), (.=?))
import qualified Toml

import Mycfg.Config.Types
import Mycfg.Errors.Types

parseConfig :: Path Abs File -> IO (Either MycfgError Config)
parseConfig configPath = do
    exists <- doesFileExist (toFilePath configPath)
    if not exists
        then return $ Left $ FilesystemError $ FileNotFound (Text.pack (toFilePath configPath))
        else parseConfigFile configPath

parseConfigFile :: Path Abs File -> IO (Either MycfgError Config)
parseConfigFile configPath = do
    result <- try $ TextIO.readFile (toFilePath configPath)
    case result of
        Left (e :: SomeException) ->
            return $ Left $ ParseError $ InvalidToml configPath (Text.pack (show e))
        Right content -> do
            case decodeConfig content of
                Left err -> return $ Left $ ParseError $ InvalidToml configPath (Text.pack err)
                Right config -> return $ Right config

decodeConfig :: Text -> Either String Config
decodeConfig content =
    case Toml.decode configCodec (LazyTextEncoding.encodeUtf8 (LazyText.fromStrict content)) of
        Left err -> Left (show err)
        Right config -> Right config

configCodec :: TomlCodec Config
configCodec =
    Config
        <$> Toml.table systemCodec "system" .= system
        <*> Toml.tableMap Toml.text Toml.text "files" .= files
        <*> Toml.table packageCodec "packages" .= packages
        <*> Toml.tableMap Toml.text serviceCodec "services" .= services
        <*> Toml.list Toml.text "modules" .= modules
        <*> Toml.tableMap Toml.text profileCodec "profiles" .= profiles

systemCodec :: TomlCodec SystemConfig
systemCodec =
    SystemConfig
        <$> Toml.text `Toml.dioptional` "hostname" .= hostname
        <*> Toml.text `Toml.dioptional` "timezone" .= timezone
        <*> Toml.text `Toml.dioptional` "locale" .= locale
        <*> Toml.text `Toml.dioptional` "shell" .= shell
        <*> Toml.text `Toml.dioptional` "editor" .= editor

packageCodec :: TomlCodec PackageConfig
packageCodec =
    PackageConfig
        <$> Toml.list Toml.text `Toml.dioptional` "cli" .= cli
        <*> Toml.list Toml.text `Toml.dioptional` "gui" .= gui
        <*> Toml.list Toml.text `Toml.dioptional` "development" .= development
        <*> Toml.list Toml.text `Toml.dioptional` "system" .= system

serviceCodec :: TomlCodec ServiceConfig
serviceCodec =
    ServiceConfig
        <*> Toml.bool .= enable
        <*> Toml.bool .= start
        <*> Toml.tableMap Toml.text Toml.text `Toml.dioptional` "config" .= config

profileCodec :: TomlCodec ProfileConfig
profileCodec =
    ProfileConfig
        <$> Toml.text .= name
        <*> Toml.text .= description
        <*> Toml.list Toml.text .= modules
        <*> Toml.list Toml.text `Toml.dioptional` "extends" .= extends

fileOperationCodec :: TomlCodec FileOperation
fileOperationCodec = Toml.enumBounded "operation"

instance Toml.HasKey FileOperation where
    hasKey = Toml.enumKey

instance Toml.FromValue FileOperation where
    fromValue = Toml.fromEnumBounded

instance Toml.ToValue FileOperation where
    toValue = Toml.toEnumBounded
