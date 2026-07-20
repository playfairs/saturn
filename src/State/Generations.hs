{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mycfg.State.Generations (
    Generation (..),
    GenerationMetadata (..),
    GenerationStatus (..),
    createGeneration,
    loadGeneration,
    saveGeneration,
    activateGeneration,
    deactivateGeneration,
    listGenerations,
    getCurrentGeneration,
    rollbackToGeneration,
    deleteGeneration,
    GenerationError (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path, toFilePath, (</>))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getDirectoryContents, removeDirectoryRecursive)
import System.FilePath ((</>))

import Mycfg.Config.Types
import Mycfg.Errors.Types
import Mycfg.State.Manifest
import Mycfg.State.Store

data Generation = Generation
    { metadata :: GenerationMetadata
    , config :: Config
    , manifest :: Manifest
    }
    deriving (Show, Eq, Generic)

instance ToJSON Generation
instance FromJSON Generation

data GenerationMetadata = GenerationMetadata
    { generationId :: Text
    , created :: UTCTime
    , description :: Text
    , version :: Text
    , status :: GenerationStatus
    , parentGeneration :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON GenerationMetadata
instance FromJSON GenerationMetadata

data GenerationStatus
    = Active
    | Inactive
    | RollingBack
    | Failed
    deriving (Show, Eq, Generic)

instance ToJSON GenerationStatus
instance FromJSON GenerationStatus

data GenerationError
    = GenerationNotFound
    | GenerationCorrupted
    | GenerationCreationFailed
    | GenerationActivationFailed
    | GenerationRollbackFailed
    | GenerationDeletionFailed
    | InvalidGenerationId
    deriving (Show, Eq)

createGeneration :: StateStore -> Config -> Text -> IO (Either GenerationError Generation)
createGeneration store config description = do
    now <- getCurrentTime
    genId <- generateGenerationId

    let metadata =
            GenerationMetadata
                { generationId = genId
                , created = now
                , description = description
                , version = "0.1.0"
                , status = Inactive
                , parentGeneration = Nothing
                }

    manifestResult <- createManifest genId description config
    case manifestResult of
        Left err -> return $ Left $ GenerationCreationFailed
        Right manifest -> do
            let generation = Generation metadata config manifest
            saveResult <- saveGeneration store generation
            case saveResult of
                Left err -> return $ Left err
                Right _ -> return $ Right generation

loadGeneration :: StateStore -> Text -> IO (Either GenerationError Generation)
loadGeneration store genId = do
    let genDir = generationsDirectory store </> Text.unpack genId
        genPath = genDir </> "generation.json"

    exists <- doesFileExist (toFilePath genPath)
    if not exists
        then return $ Left GenerationNotFound
        else do
            content <- readFile (toFilePath genPath)
            case decode (LazyByteString.pack content) of
                Nothing -> return $ Left GenerationCorrupted
                Just generation -> return $ Right generation

saveGeneration :: StateStore -> Generation -> IO (Either GenerationError ())
saveGeneration store generation = do
    let genId = generationId (metadata generation)
        genDir = generationsDirectory store </> Text.unpack genId
        genPath = genDir </> "generation.json"
        manifestPath = genDir </> "manifest.json"

    createDirectoryIfMissing True (toFilePath genDir)

    let genContent = encode generation
        manifestContent = encode (manifest generation)

    genResult <- try $ writeFile (toFilePath genPath) (LazyByteString.unpack genContent)
    case genResult of
        Left (_ :: SomeException) -> return $ Left GenerationCreationFailed
        Right _ -> do
            manifestResult <- try $ writeFile (toFilePath manifestPath) (LazyByteString.unpack manifestContent)
            case manifestResult of
                Left (_ :: SomeException) -> return $ Left GenerationCreationFailed
                Right _ -> return $ Right ()

activateGeneration :: StateStore -> Text -> IO (Either GenerationError ())
activateGeneration store genId = do
    genResult <- loadGeneration store genId
    case genResult of
        Left err -> return $ Left err
        Right generation -> do
            let currentGenPath = stateDirectory store </> "current-generation"
                newGen = generation{metadata = (metadata generation){status = Active}}

            saveResult <- saveGeneration store newGen
            case saveResult of
                Left err -> return $ Left err
                Right _ -> do
                    result <- try $ writeFile (toFilePath currentGenPath) (Text.unpack genId)
                    case result of
                        Left (_ :: SomeException) -> return $ Left GenerationActivationFailed
                        Right _ -> return $ Right ()

deactivateGeneration :: StateStore -> Text -> IO (Either GenerationError ())
deactivateGeneration store genId = do
    genResult <- loadGeneration store genId
    case genResult of
        Left err -> return $ Left err
        Right generation -> do
            let newGen = generation{metadata = (metadata generation){status = Inactive}}
            saveGeneration store newGen

listGenerations :: StateStore -> IO (Either GenerationError [Generation])
listGenerations store = do
    let genDir = generationsDirectory store
        genPath = toFilePath genDir

    exists <- doesDirectoryExist genPath
    if not exists
        then return $ Right []
        else do
            entries <- getDirectoryContents genPath
            let genIds = filter (`notElem` [".", ".."]) entries

            genResults <- mapM (loadGeneration store . Text.pack) genIds
            let (errors, generations) = partitionEithers genResults

            if null errors
                then return $ Right generations
                else return $ Left $ head errors

getCurrentGeneration :: StateStore -> IO (Either GenerationError (Maybe Generation))
getCurrentGeneration store = do
    let currentGenPath = stateDirectory store </> "current-generation"

    exists <- doesFileExist (toFilePath currentGenPath)
    if not exists
        then return $ Right Nothing
        else do
            content <- readFile (toFilePath currentGenPath)
            let genId = Text.strip $ Text.pack content
            loadGeneration store genId

rollbackToGeneration :: StateStore -> Text -> IO (Either GenerationError ())
rollbackToGeneration store genId = do
    currentGen <- getCurrentGeneration store
    case currentGen of
        Right (Just current) -> do
            let currentGenId = generationId (metadata current)
                updatedCurrent = current{metadata = (metadata current){status = Inactive}}
            saveGeneration store updatedCurrent
        Right Nothing -> return $ Right ()
        Left err -> return $ Left err

    targetGen <- loadGeneration store genId
    case targetGen of
        Left err -> return $ Left err
        Right target -> do
            let rollingBackGen = target{metadata = (metadata target){status = RollingBack}}
            saveGeneration store rollingBackGen

            activateResult <- activateGeneration store genId
            case activateResult of
                Left err -> do
                    let failedGen = rollingBackGen{metadata = (metadata rollingBackGen){status = Failed}}
                    saveGeneration store failedGen
                    return $ Left err
                Right _ -> do
                    let activeGen = rollingBackGen{metadata = (metadata rollingBackGen){status = Active}}
                    saveGeneration store activeGen
                    return $ Right ()

deleteGeneration :: StateStore -> Text -> IO (Either GenerationError ())
deleteGeneration store genId = do
    currentGen <- getCurrentGeneration store
    case currentGen of
        Right (Just current)
            | generationId (metadata current) == genId ->
                return $ Left GenerationDeletionFailed
        _ -> do
            let genDir = generationsDirectory store </> Text.unpack genId
            exists <- doesDirectoryExist (toFilePath genDir)
            if not exists
                then return $ Left GenerationNotFound
                else do
                    result <- try $ removeDirectoryRecursive (toFilePath genDir)
                    case result of
                        Left (_ :: SomeException) -> return $ Left GenerationDeletionFailed
                        Right _ -> return $ Right ()

generateGenerationId :: IO Text
generateGenerationId = do
    uuid <- nextRandom
    return $ "gen-" <> Text.pack (UUID.toString uuid)

validateGeneration :: Generation -> Either GenerationError ()
validateGeneration generation = do
    let genId = generationId (metadata generation)
    if Text.null genId
        then Left InvalidGenerationId
        else Right ()

getGenerationPath :: StateStore -> Text -> Path Abs Dir
getGenerationPath store genId =
    generationsDirectory store </> Text.unpack genId

getGenerationManifestPath :: StateStore -> Text -> Path Abs File
getGenerationManifestPath store genId =
    getGenerationPath store genId </> "manifest.json"

getGenerationConfigPath :: StateStore -> Text -> Path Abs File
getGenerationConfigPath store genId =
    getGenerationPath store genId </> "config.json"

cleanupOldGenerations :: StateStore -> Int -> IO (Either GenerationError ())
cleanupOldGenerations store maxGens = do
    genList <- listGenerations store
    case genList of
        Left err -> return $ Left err
        Right generations -> do
            let sortedGens = sortBy (flip compare `on` (created . metadata)) generations
                toRemove = drop maxGens sortedGens
            results <- mapM (\gen -> deleteGeneration store (generationId (metadata gen))) toRemove
            let errors = [err | Left err <- results]
            if null errors
                then return $ Right ()
                else return $ Left $ head errors

getGenerationStatistics :: StateStore -> IO (Either GenerationError (Map GenerationStatus Int))
getGenerationStatistics store = do
    genList <- listGenerations store
    case genList of
        Left err -> return $ Left err
        Right generations -> do
            let statusCounts =
                    Map.fromListWith
                        (+)
                        [(status (metadata gen), 1) | gen <- generations]
            return $ Right statusCounts
