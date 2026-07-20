{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mycfg.Modules.Registry (
    ModuleRegistry (..),
    RegistryEntry (..),
    RegistryConfig (..),
    createRegistry,
    registerModule,
    unregisterModule,
    findModule,
    listModules,
    searchModules,
    validateRegistry,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path, toFilePath, (</>))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

import Mycfg.Config.Types
import Mycfg.Modules.Loader

data ModuleRegistry = ModuleRegistry
    { config :: RegistryConfig
    , entries :: Map Text RegistryEntry
    , index :: ModuleIndex
    , loadedAt :: UTCTime
    }
    deriving (Show, Eq, Generic)

instance ToJSON ModuleRegistry
instance FromJSON ModuleRegistry

data RegistryEntry = RegistryEntry
    { moduleInfo :: ModuleInfo
    , registeredAt :: UTCTime
    , enabled :: Bool
    , priority :: Int
    , tags :: Set Text
    , metadata :: Map Text Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON RegistryEntry
instance FromJSON RegistryEntry

data RegistryConfig = RegistryConfig
    { registryPath :: Path Abs Dir
    , autoIndex :: Bool
    , validateOnRegister :: Bool
    , maxEntries :: Int
    }
    deriving (Show, Eq, Generic)

instance ToJSON RegistryConfig
instance FromJSON RegistryConfig

data ModuleIndex = ModuleIndex
    { byName :: Map Text (Set Text)
    , byTag :: Map Text (Set Text)
    , byDependency :: Map Text (Set Text)
    , byProvider :: Map Text (Set Text)
    }
    deriving (Show, Eq, Generic)

instance ToJSON ModuleIndex
instance FromJSON ModuleIndex

createRegistry :: RegistryConfig -> IO ModuleRegistry
createRegistry registryConfig = do
    now <- getCurrentTime
    let registryPath = registryPath registryConfig

    createDirectoryIfMissing True (toFilePath registryPath)

    let index =
            ModuleIndex
                { byName = Map.empty
                , byTag = Map.empty
                , byDependency = Map.empty
                , byProvider = Map.empty
                }

    return $
        ModuleRegistry
            { config = registryConfig
            , entries = Map.empty
            , index = index
            , loadedAt = now
            }

registerModule :: ModuleRegistry -> ModuleInfo -> Set Text -> Map Text Text -> IO (Either Text ModuleRegistry)
registerModule registry moduleInfo tags metadata = do
    let moduleName = name moduleInfo
        registryConfig = config registry
        currentEntries = entries registry

    if Map.member moduleName currentEntries
        then return $ Left $ "Module already registered: " <> moduleName
        else
            if Map.size currentEntries >= maxEntries registryConfig
                then return $ Left "Registry is full"
                else do
                    now <- getCurrentTime
                    let entry =
                            RegistryEntry
                                { moduleInfo = moduleInfo
                                , registeredAt = now
                                , enabled = True
                                , priority = 0
                                , tags = tags
                                , metadata = metadata
                                }

                    let newEntries = Map.insert moduleName entry currentEntries
                        newIndex = updateIndex (index registry) moduleInfo tags

                    let newRegistry =
                            registry
                                { entries = newEntries
                                , index = newIndex
                                }

                    if validateOnRegister registryConfig
                        then do
                            validationResult <- validateModule moduleInfo
                            case validationResult of
                                Left err -> return $ Left $ "Module validation failed: " <> err
                                Right _ -> return $ Right newRegistry
                        else return $ Right newRegistry

unregisterModule :: ModuleRegistry -> Text -> IO (Either Text ModuleRegistry)
unregisterModule registry moduleName = do
    let currentEntries = entries registry

    case Map.lookup moduleName currentEntries of
        Nothing -> return $ Left $ "Module not found: " <> moduleName
        Just _ -> do
            let newEntries = Map.delete moduleName currentEntries
                entry = currentEntries Map.! moduleName
                newIndex = removeFromIndex (index registry) (moduleInfo entry) (tags entry)

            let newRegistry =
                    registry
                        { entries = newEntries
                        , index = newIndex
                        }

            return $ Right newRegistry

findModule :: ModuleRegistry -> Text -> Maybe RegistryEntry
findModule registry moduleName = Map.lookup moduleName (entries registry)

listModules :: ModuleRegistry -> [RegistryEntry]
listModules registry = Map.elems (entries registry)

searchModules :: ModuleRegistry -> Text -> [RegistryEntry]
searchModules registry query =
    let allEntries = Map.elems (entries registry)
        indexedResults = searchByIndex (index registry) query
        directResults = filter (matchesQuery query) allEntries
        combinedResults = indexedResults ++ directResults
     in nub combinedResults

searchByIndex :: ModuleIndex -> Text -> [RegistryEntry]
searchByIndex index query =
    let nameResults = Map.lookup query (byName index)
        tagResults = Map.lookup query (byTag index)
        depResults = Map.lookup query (byDependency index)
        providerResults = Map.lookup query (byProvider index)
        allResults = Set.unions [nameResults, tagResults, depResults, providerResults]
     in Set.toList allResults

matchesQuery :: Text -> RegistryEntry -> Bool
matchesQuery query entry =
    let info = moduleInfo entry
        entryTags = tags entry
     in query `Text.isInfixOf` name info
            || query `Text.isInfixOf` description info
            || query `Text.isInfixOf` version info
            || query `Set.member` entryTags
            || any (query `Text.isInfixOf`) (provides info)
            || any (query `Text.isInfixOf`) (dependencies info)

updateIndex :: ModuleIndex -> ModuleInfo -> Set Text -> ModuleIndex
updateIndex index moduleInfo tags =
    let moduleName = name moduleInfo
        moduleDeps = Set.fromList (dependencies moduleInfo)
        moduleProvides = Set.fromList (provides moduleInfo)

        newByName = Map.insertWith Set.union moduleName (Set.singleton moduleName) (byName index)
        newByTag = Set.foldl' (\acc tag -> Map.insertWith Set.union tag (Set.singleton moduleName) acc) (byTag index) tags
        newByDep = Set.foldl' (\acc dep -> Map.insertWith Set.union dep (Set.singleton moduleName) acc) (byDependency index) moduleDeps
        newByProvider = Set.foldl' (\acc provider -> Map.insertWith Set.union provider (Set.singleton moduleName) acc) (byProvider index) moduleProvides
     in ModuleIndex newByName newByTag newByDep newByProvider

removeFromIndex :: ModuleIndex -> ModuleInfo -> Set Text -> ModuleIndex
removeFromIndex index moduleInfo tags =
    let moduleName = name moduleInfo
        moduleDeps = Set.fromList (dependencies moduleInfo)
        moduleProvides = Set.fromList (provides moduleInfo)

        removeFromMap :: Map Text (Set Text) -> Text -> Map Text (Set Text)
        removeFromMap m key = Map.adjust (Set.delete moduleName) key

        newByName = Map.delete moduleName (byName index)
        newByTag = Set.foldl' removeFromMap (byTag index) tags
        newByDep = Set.foldl' removeFromMap (byDependency index) moduleDeps
        newByProvider = Set.foldl' removeFromMap (byProvider index) moduleProvides
     in ModuleIndex newByName newByTag newByDep newByProvider

validateRegistry :: ModuleRegistry -> IO (Either Text ())
validateRegistry registry = do
    let allEntries = Map.elems (entries registry)
        moduleNames = Map.keys (entries registry)

    let duplicateNames = findDuplicates moduleNames
    unless (null duplicateNames) $
        return $
            Left $
                "Duplicate module names: " <> Text.intercalate ", " duplicateNames

    let validationResults = map validateRegistryEntry allEntries
        errors = [err | Left err <- validationResults]

    if null errors
        then return $ Right ()
        else return $ Left $ "Registry validation failed: " <> Text.intercalate ", " errors

validateRegistryEntry :: RegistryEntry -> Either Text ()
validateRegistryEntry entry = do
    let info = moduleInfo entry

    when (Text.null $ name info) $
        Left "Module name cannot be empty"

    when (Text.null $ version info) $
        Left "Module version cannot be empty"

    when (Text.null $ description info) $
        Left "Module description cannot be empty"

    when (priority entry < 0) $
        Left "Priority cannot be negative"

    Right ()

findDuplicates :: [Text] -> [Text]
findDuplicates xs =
    let grouped = group xs
        duplicates = filter ((> 1) . length) grouped
     in map head duplicates

group :: (Eq a) => [a] -> [[a]]
group [] = []
group (x : xs) =
    let (ys, zs) = span (== x) xs
     in (x : ys) : group zs

enableModule :: ModuleRegistry -> Text -> IO (Either Text ModuleRegistry)
enableModule registry moduleName = do
    let currentEntries = entries registry

    case Map.lookup moduleName currentEntries of
        Nothing -> return $ Left $ "Module not found: " <> moduleName
        Just entry -> do
            let updatedEntry = entry{enabled = True}
                newEntries = Map.insert moduleName updatedEntry currentEntries

            let newRegistry = registry{entries = newEntries}
            return $ Right newRegistry

disableModule :: ModuleRegistry -> Text -> IO (Either Text ModuleRegistry)
disableModule registry moduleName = do
    let currentEntries = entries registry

    case Map.lookup moduleName currentEntries of
        Nothing -> return $ Left $ "Module not found: " <> moduleName
        Just entry -> do
            let updatedEntry = entry{enabled = False}
                newEntries = Map.insert moduleName updatedEntry currentEntries

            let newRegistry = registry{entries = newEntries}
            return $ Right newRegistry

setModulePriority :: ModuleRegistry -> Text -> Int -> IO (Either Text ModuleRegistry)
setModulePriority registry moduleName newPriority = do
    let currentEntries = entries registry

    case Map.lookup moduleName currentEntries of
        Nothing -> return $ Left $ "Module not found: " <> moduleName
        Just entry -> do
            let updatedEntry = entry{priority = newPriority}
                newEntries = Map.insert moduleName updatedEntry currentEntries

            let newRegistry = registry{entries = newEntries}
            return $ Right newRegistry

addModuleTags :: ModuleRegistry -> Text -> Set Text -> IO (Either Text ModuleRegistry)
addModuleTags registry moduleName newTags = do
    let currentEntries = entries registry

    case Map.lookup moduleName currentEntries of
        Nothing -> return $ Left $ "Module not found: " <> moduleName
        Just entry -> do
            let currentTags = tags entry
                updatedTags = Set.union currentTags newTags
                updatedEntry = entry{tags = updatedTags}
                newEntries = Map.insert moduleName updatedEntry currentEntries

            let oldIndex = index registry
                info = moduleInfo entry
                newIndex = updateIndex oldIndex info updatedTags

            let newRegistry =
                    registry
                        { entries = newEntries
                        , index = newIndex
                        }

            return $ Right newRegistry

removeModuleTags :: ModuleRegistry -> Text -> Set Text -> IO (Either Text ModuleRegistry)
removeModuleTags registry moduleName tagsToRemove = do
    let currentEntries = entries registry

    case Map.lookup moduleName currentEntries of
        Nothing -> return $ Left $ "Module not found: " <> moduleName
        Just entry -> do
            let currentTags = tags entry
                updatedTags = Set.difference currentTags tagsToRemove
                updatedEntry = entry{tags = updatedTags}
                newEntries = Map.insert moduleName updatedEntry currentEntries

            let oldIndex = index registry
                info = moduleInfo entry
                newIndex = removeFromIndex oldIndex info currentTags

            let newRegistry =
                    registry
                        { entries = newEntries
                        , index = newIndex
                        }

            return $ Right newRegistry

getRegistryStatistics :: ModuleRegistry -> Map Text Int
getRegistryStatistics registry =
    let allEntries = Map.elems (entries registry)
        enabledCount = length $ filter enabled allEntries
        disabledCount = length allEntries - enabledCount
        totalTags = Set.size $ Set.unions $ map tags allEntries
        totalDeps = Set.size $ Set.unions $ map (Set.fromList . dependencies . moduleInfo) allEntries
        totalProviders = Set.size $ Set.unions $ map (Set.fromList . provides . moduleInfo) allEntries
     in Map.fromList
            [ ("total_modules", length allEntries)
            , ("enabled_modules", enabledCount)
            , ("disabled_modules", disabledCount)
            , ("total_tags", totalTags)
            , ("total_dependencies", totalDeps)
            , ("total_providers", totalProviders)
            ]

exportRegistry :: ModuleRegistry -> Text
exportRegistry registry =
    let allEntries = Map.elems (entries registry)
        entryLines = map exportRegistryEntry allEntries
     in Text.unlines entryLines

exportRegistryEntry :: RegistryEntry -> Text
exportRegistryEntry entry =
    let info = moduleInfo entry
        tagsStr = Text.intercalate "," (Set.toList $ tags entry)
        depsStr = Text.intercalate "," (dependencies info)
        providesStr = Text.intercalate "," (provides info)
        enabledStr = if enabled entry then "enabled" else "disabled"
     in Text.unwords
            [ name info
            , version info
            , description info
            , enabledStr
            , Text.pack $ show $ priority entry
            , tagsStr
            , depsStr
            , providesStr
            ]

importRegistry :: Text -> IO (Either Text ModuleRegistry)
importRegistry registryText = do
    let lines' = Text.lines registryText
        entries = mapMaybe importRegistryEntry lines'

    registryConfig <- createDefaultRegistryConfig
    foldM (addEntryToRegistry registryConfig) (createRegistry registryConfig) entries

createDefaultRegistryConfig :: IO RegistryConfig
createDefaultRegistryConfig = do
    homeDir <- getHomeDirectory
    case parseAbsDir (homeDir </> ".local" </> "share" </> "mycfg" </> "registry") of
        Left _ -> error "Failed to create default registry config"
        Right path ->
            return $
                RegistryConfig
                    { registryPath = path
                    , autoIndex = True
                    , validateOnRegister = True
                    , maxEntries = 1000
                    }

addEntryToRegistry :: RegistryConfig -> ModuleRegistry -> ModuleInfo -> IO (Either Text ModuleRegistry)
addEntryToRegistry registryConfig registry info = do
    registerModule registry info Set.empty Map.empty

importRegistryEntry :: Text -> Maybe ModuleInfo
importRegistryEntry line =
    let parts = Text.words line
     in case parts of
            [name', version', description', enabledStr, priorityStr, tagsStr, depsStr, providesStr] -> do
                priority <- readMaybe (Text.unpack priorityStr)
                let enabled = enabledStr == "enabled"
                    tags = Set.fromList $ Text.splitOn "," tagsStr
                    deps = Text.splitOn "," depsStr
                    provides = Text.splitOn "," providesStr

                Just $
                    ModuleInfo
                        { name = name'
                        , version = version'
                        , description = description'
                        , author = ""
                        , license = ""
                        , homepage = Nothing
                        , dependencies = deps
                        , provides = provides
                        , conflicts = []
                        }
            _ -> Nothing
