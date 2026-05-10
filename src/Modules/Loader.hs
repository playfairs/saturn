{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}

module Mycfg.Modules.Loader
  ( ModuleLoader(..)
  , LoadContext(..)
  , LoadResult(..)
  , LoadError(..)
  , loadModule
  , loadModules
  , loadModuleDependencies
  , validateModule
  ) where

import Control.Exception (bracket, bracketOnError, try, SomeException)
import Control.Monad (when, unless)
import Control.Monad.IO.Class
import Data.Aeson (ToJSON, FromJSON)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Path (Abs, File, Dir, Path, toFilePath, (</>))
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeDirectory, takeExtension)

import Mycfg.Config.Types
import Mycfg.Errors.Types

data ModuleLoader = ModuleLoader
  { modulePaths :: [Path Abs Dir]
  , loadedModules :: Map Text LoadedModule
  , loadCache :: Map Text ModuleInfo
  }

data LoadContext = LoadContext
  { loader :: ModuleLoader
  , visitedModules :: Set Text
  , loadingStack :: [Text]
  }

data LoadResult
  = LoadSuccess LoadedModule
  | LoadFailure LoadError
  deriving (Show, Eq)

data LoadError
  = ModuleNotFound Text
  | InvalidModulePath Text
  | ModuleParseError Text Text
  | ModuleValidationError Text Text
  | CircularDependency [Text]
  | DependencyNotFound Text Text
  | PermissionDenied Text
  deriving (Show, Eq, Generic)

instance ToJSON LoadError
instance FromJSON LoadError

data LoadedModule = LoadedModule
  { moduleInfo :: ModuleInfo
  , moduleConfig :: ModuleConfig
  , loadedAt :: UTCTime
  , modulePath :: Path Abs File
  } deriving (Show, Eq, Generic)

instance ToJSON LoadedModule
instance FromJSON LoadedModule

data ModuleInfo = ModuleInfo
  { name :: Text
  , version :: Text
  , description :: Text
  , author :: Text
  , license :: Text
  , homepage :: Maybe Text
  , dependencies :: [Text]
  , provides :: [Text]
  , conflicts :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON ModuleInfo
instance FromJSON ModuleInfo

loadModule :: ModuleLoader -> Text -> IO LoadResult
loadModule loader moduleName = do
  let loadContext = LoadContext
        { loader = loader
        , visitedModules = Set.empty
        , loadingStack = []
        }
  
  loadModuleWithContext loadContext moduleName

loadModules :: ModuleLoader -> [Text] -> IO [LoadResult]
loadModules loader moduleNames = do
  let loadContext = LoadContext
        { loader = loader
        , visitedModules = Set.empty
        , loadingStack = []
        }
  
  mapM (loadModuleWithContext loadContext) moduleNames

loadModuleDependencies :: ModuleLoader -> Text -> IO LoadResult
loadModuleDependencies loader moduleName = do
  let loadContext = LoadContext
        { loader = loader
        , visitedModules = Set.empty
        , loadingStack = [moduleName]
        }
  
  loadModuleWithDependencies loadContext moduleName

loadModuleWithContext :: LoadContext -> Text -> IO LoadResult
loadModuleWithContext context moduleName = do
  let visited = visitedModules context
      stack = loadingStack context
      loader' = loader context
  
  if moduleName `Set.member` visited
    then return $ LoadSuccess $ getLoadedModule loader' moduleName
    else if moduleName `elem` stack
      then return $ LoadFailure $ CircularDependency (moduleName : stack)
      else do
        let newContext = context
              { visitedModules = Set.insert moduleName visited
              , loadingStack = moduleName : stack
              }
        
        loadModuleInternal newContext moduleName

loadModuleInternal :: LoadContext -> Text -> IO LoadResult
loadModuleInternal context moduleName = do
  let loader' = loader context
  
  moduleResult <- findModuleFile loader' moduleName
  case moduleResult of
    Left err -> return $ LoadFailure err
    Right modulePath -> do
      loadResult <- loadModuleFromFile modulePath
      case loadResult of
        Left err -> return $ LoadFailure $ ModuleParseError moduleName err
        Right moduleInfo -> do
          validationResult <- validateModule moduleInfo
          case validationResult of
            Left err -> return $ LoadFailure $ ModuleValidationError moduleName err
            Right _ -> do
              loadedModule <- createLoadedModule moduleInfo modulePath
              return $ LoadSuccess loadedModule

loadModuleWithDependencies :: LoadContext -> Text -> IO LoadResult
loadModuleWithDependencies context moduleName = do
  let loader' = loader context
  
  moduleResult <- loadModuleWithContext context moduleName
  case moduleResult of
    LoadFailure err -> return $ LoadFailure err
    LoadSuccess loadedModule -> do
      let dependencies = dependencies $ moduleInfo loadedModule
          newContext = context
                { loader = loader' { loadedModules = Map.insert moduleName loadedModule (loadedModules loader') }
                }
      
      dependencyResults <- mapM (loadModuleWithDependencies newContext) dependencies
      let failures = [err | LoadFailure err <- dependencyResults]
      
      if null failures
        then return $ LoadSuccess loadedModule
        else return $ LoadFailure $ head failures

findModuleFile :: ModuleLoader -> Text -> IO (Either LoadError (Path Abs File))
findModuleFile loader moduleName = do
  let paths = modulePaths loader
      possibleNames = [moduleName <> ".toml", moduleName <> "/module.toml", moduleName <> "/mycfg.toml"]
  
  results <- mapM (searchInPaths possibleNames) paths
  let found = [path | Right path <- results]
  
  if null found
    then return $ Left $ ModuleNotFound moduleName
    else return $ Right $ head found

searchInPaths :: [Text] -> Path Abs Dir -> IO (Either LoadError (Path Abs File))
searchInPaths names basePath = do
  results <- mapM (searchInPath basePath) names
  let found = [path | Right path <- results]
  
  if null found
    then return $ Left $ ModuleNotFound ""
    else return $ Right $ head found

searchInPath :: Path Abs Dir -> Text -> IO (Either LoadError (Path Abs File))
searchInPath basePath name = do
  let pathStr = Text.unpack name
      fullPath = toFilePath basePath </> pathStr
  
  exists <- doesFileExist fullPath
  if exists
    then case parseAbsFile fullPath of
      Left _ -> return $ Left $ InvalidModulePath name
      Right path -> return $ Right path
    else return $ Left $ ModuleNotFound name

loadModuleFromFile :: Path Abs File -> IO (Either Text ModuleInfo)
loadModuleFromFile modulePath = do
  let moduleFile = toFilePath modulePath
  
  content <- try $ TextIO.readFile moduleFile
  case content of
    Left (e :: SomeException) -> return $ Left $ Text.pack $ show e
    Right text -> do
      case decodeModuleInfo text of
        Left err -> return $ Left err
        Right info -> return $ Right info

decodeModuleInfo :: Text -> Either Text ModuleInfo
decodeModuleInfo content = do
  case decode (LazyByteString.pack (Text.unpack content)) of
    Nothing -> Left "Failed to decode module info"
    Just info -> Right info

validateModule :: ModuleInfo -> Either Text ()
validateModule moduleInfo = do
  when (Text.null $ name moduleInfo) $
    Left "Module name cannot be empty"
  
  when (Text.null $ version moduleInfo) $
    Left "Module version cannot be empty"
  
  when (Text.null $ description moduleInfo) $
    Left "Module description cannot be empty"
  
  when (any Text.null (dependencies moduleInfo)) $
    Left "Dependency names cannot be empty"
  
  when (any Text.null (provides moduleInfo)) $
    Left "Provides names cannot be empty"
  
  when (any Text.null (conflicts moduleInfo)) $
    Left "Conflict names cannot be empty"
  
  Right ()

createLoadedModule :: ModuleInfo -> Path Abs File -> IO LoadedModule
createLoadedModule moduleInfo modulePath = do
  now <- getCurrentTime
  let moduleConfig = ModuleConfig
        { name = name moduleInfo
        , description = description moduleInfo
        , version = version moduleInfo
        , dependencies = dependencies moduleInfo
        , config = Map.empty
        }
  
  return $ LoadedModule
    { moduleInfo = moduleInfo
    , moduleConfig = moduleConfig
    , loadedAt = now
    , modulePath = modulePath
    }

getLoadedModule :: ModuleLoader -> Text -> LoadedModule
getLoadedModule loader moduleName = 
  case Map.lookup moduleName (loadedModules loader) of
    Just module' -> module'
    Nothing -> error $ "Module not found in cache: " ++ Text.unpack moduleName

validateModuleDependencies :: ModuleLoader -> [Text] -> IO (Either LoadError ())
validateModuleDependencies loader moduleNames = do
  dependencyResults <- mapM (validateSingleDependency loader) moduleNames
  let failures = [err | Left err <- dependencyResults]
  
  if null failures
    then return $ Right ()
    else return $ Left $ head failures

validateSingleDependency :: ModuleLoader -> Text -> IO (Either LoadError ())
validateSingleDependency loader moduleName = do
  moduleResult <- findModuleFile loader moduleName
  case moduleResult of
    Left err -> return $ Left err
    Right _ -> return $ Right ()

createModuleLoader :: [Path Abs Dir] -> IO ModuleLoader
createModuleLoader paths = do
  return $ ModuleLoader
    { modulePaths = paths
    , loadedModules = Map.empty
    , loadCache = Map.empty
    }

addModulePath :: ModuleLoader -> Path Abs Dir -> ModuleLoader
addModulePath loader path = loader
  { modulePaths = path : modulePaths loader }

removeModulePath :: ModuleLoader -> Path Abs Dir -> ModuleLoader
removeModulePath loader path = loader
  { modulePaths = filter (/= path) (modulePaths loader) }

clearModuleCache :: ModuleLoader -> ModuleLoader
clearModuleCache loader = loader
  { loadedModules = Map.empty
  , loadCache = Map.empty
  }

getModuleStatistics :: ModuleLoader -> Map Text Int
getModuleStatistics loader = 
  let loadedCount = Map.size (loadedModules loader)
      pathCount = length (modulePaths loader)
      cacheCount = Map.size (loadCache loader)
  in Map.fromList
    [ ("loaded_modules", loadedCount)
    , ("module_paths", pathCount)
    , ("cached_modules", cacheCount)
    ]

searchModules :: ModuleLoader -> Text -> IO [ModuleInfo]
searchModules loader query = do
  let paths = modulePaths loader
  
  allModules <- concatMap (findModulesInPath paths) paths
  let matching = filter (matchesQuery query) allModules
  
  return matching

findModulesInPath :: [Path Abs Dir] -> Path Abs Dir -> IO [ModuleInfo]
findModulesInPath allPaths basePath = do
  exists <- doesDirectoryExist (toFilePath basePath)
  if not exists
    then return []
    else do
      entries <- getDirectoryContents (toFilePath basePath)
      let moduleFiles = filter isModuleFile entries
      
      moduleInfos <- mapM (loadModuleInfoFromPath basePath) moduleFiles
      return $ [info | Right info <- moduleInfos]

isModuleFile :: FilePath -> Bool
isModuleFile entry = 
  takeExtension entry == ".toml" || 
  entry == "module.toml" || 
  entry == "mycfg.toml"

loadModuleInfoFromPath :: Path Abs Dir -> FilePath -> IO (Either Text ModuleInfo)
loadModuleInfoFromPath basePath entry = do
  let fullPath = toFilePath basePath </> entry
  
  case parseAbsFile fullPath of
    Left _ -> return $ Left $ "Invalid path: " <> Text.pack fullPath
    Right path -> do
      result <- loadModuleFromFile path
      case result of
        Left err -> return $ Left err
        Right info -> return $ Right info

matchesQuery :: Text -> ModuleInfo -> Bool
matchesQuery query moduleInfo = 
  query `Text.isInfixOf` name moduleInfo ||
  query `Text.isInfixOf` description moduleInfo ||
  any (query `Text.isInfixOf`) (provides moduleInfo)
