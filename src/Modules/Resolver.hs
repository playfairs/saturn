{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}

module Mycfg.Modules.Resolver
  ( DependencyResolver(..)
  , ResolutionContext(..)
  , ResolutionResult(..)
  , ResolutionError(..)
  , DependencyGraph(..)
  , ResolutionPlan(..)
  , resolveDependencies
  , buildDependencyGraph
  , detectCircularDependencies
  , topologicalSort
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Graph (Graph, buildG, topSort, reachable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.List (sort, nub, groupBy, sortBy)
import Data.Ord (comparing)
import GHC.Generics (Generic)
import Path (Abs, File, Dir, Path, toFilePath)

import Mycfg.Config.Types
import Mycfg.Modules.Loader
import Mycfg.Modules.Registry

data DependencyResolver = DependencyResolver
  { moduleLoader :: ModuleLoader
  , moduleRegistry :: ModuleRegistry
  , resolutionCache :: Map Text (Set Text)
  }

data ResolutionContext = ResolutionContext
  { resolver :: DependencyResolver
  , requestedModules :: Set Text
  , visitedModules :: Set Text
  , resolutionStack :: [Text]
  }

data ResolutionResult
  = ResolutionSuccess ResolutionPlan
  | ResolutionFailure ResolutionError
  deriving (Show, Eq)

data ResolutionError
  | ModuleNotFound Text
  | CircularDependency [Text]
  | ConflictDetected [Text]
  | DependencyNotFound Text Text
  | ResolutionTimeout
  | InvalidDependency Text
  deriving (Show, Eq, Generic)

instance ToJSON ResolutionError
instance FromJSON ResolutionError

data DependencyGraph = DependencyGraph
  { graph :: Graph
  , vertexMap :: Map Text Int
  , reverseVertexMap :: Map Int Text
  , edges :: [(Text, Text)]
  } deriving (Show, Eq, Generic)

instance ToJSON DependencyGraph
instance FromJSON DependencyGraph

data ResolutionPlan = ResolutionPlan
  { resolvedModules :: [Text]
  , dependencyOrder :: [Text]
  , conflicts :: [Text]
  , warnings :: [Text]
  , metadata :: ResolutionMetadata
  } deriving (Show, Eq, Generic)

instance ToJSON ResolutionPlan
instance FromJSON ResolutionPlan

data ResolutionMetadata = ResolutionMetadata
  { totalModules :: Int
  , totalDependencies :: Int
  , resolutionTime :: Int
  , cacheHits :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON ResolutionMetadata
instance FromJSON ResolutionMetadata

resolveDependencies :: DependencyResolver -> [Text] -> IO ResolutionResult
resolveDependencies resolver moduleNames = do
  let context = ResolutionContext
        { resolver = resolver
        , requestedModules = Set.fromList moduleNames
        , visitedModules = Set.empty
        , resolutionStack = []
        }
  
  resolutionResults <- mapM (resolveModule context) moduleNames
  let (successes, failures) = partitionEithers resolutionResults
  
  if null failures
    then do
      let allResolved = Set.unions successes
          graphResult <- buildDependencyGraph resolver (Set.toList allResolved)
      case graphResult of
        Left err -> return $ ResolutionFailure err
        Right depGraph -> do
          sortResult <- topologicalSort depGraph
          case sortResult of
            Left err -> return $ ResolutionFailure err
            Right sorted -> do
              let plan = ResolutionPlan
                    { resolvedModules = Set.toList allResolved
                    , dependencyOrder = sorted
                    , conflicts = []
                    , warnings = []
                    , metadata = ResolutionMetadata
                        { totalModules = Set.size allResolved
                        , totalDependencies = 0
                        , resolutionTime = 0
                        , cacheHits = 0
                        }
                    }
              return $ ResolutionSuccess plan
    else return $ ResolutionFailure $ head failures

resolveModule :: ResolutionContext -> Text -> IO (Either ResolutionError (Set Text))
resolveModule context moduleName = do
  let visited = visitedModules context
      stack = resolutionStack context
      resolver' = resolver context
  
  if moduleName `Set.member` visited
    then return $ Right Set.empty
    else if moduleName `elem` stack
      then return $ Left $ CircularDependency (moduleName : stack)
      else do
        let newContext = context
              { visitedModules = Set.insert moduleName visited
              , resolutionStack = moduleName : stack
              }
        
        moduleResult <- findModuleInResolver resolver' moduleName
        case moduleResult of
          Left err -> return $ Left err
          Right moduleInfo -> do
            let dependencies = dependencies moduleInfo
            dependencyResults <- mapM (resolveModule newContext) dependencies
            let (successes, failures) = partitionEithers dependencyResults
            
            if null failures
              then do
                let allDeps = Set.unions (Set.singleton moduleName : successes)
                return $ Right allDeps
              else return $ Left $ head failures

findModuleInResolver :: DependencyResolver -> Text -> IO (Either ResolutionError ModuleInfo)
findModuleInResolver resolver moduleName = do
  let loader = moduleLoader resolver
      registry = moduleRegistry resolver
  
  registryResult <- return $ findModule registry moduleName
  case registryResult of
    Just entry -> return $ Right $ moduleInfo entry
    Nothing -> do
      loadResult <- loadModule loader moduleName
      case loadResult of
        LoadSuccess loadedModule -> return $ Right $ moduleInfo loadedModule
        LoadFailure err -> return $ Left $ ModuleNotFound moduleName

buildDependencyGraph :: DependencyResolver -> [Text] -> IO (Either ResolutionError DependencyGraph)
buildDependencyGraph resolver moduleNames = do
  allModules <- mapM (findModuleInResolver resolver) moduleNames
  let (successes, failures) = partitionEithers allModules
  
  if null failures
    then do
      let moduleInfos = zip moduleNames successes
          allDeps = concatMap (\(name, info) -> map (\dep -> (dep, name)) (dependencies info)) moduleInfos
          allNodes = moduleNames ++ nub (map fst allDeps)
          vertexMap' = Map.fromList $ zip allNodes [0..]
          reverseVertexMap' = Map.fromList $ zip [0..] allNodes
          edges' = map (\(from, to) -> (vertexMap' Map.! from, vertexMap' Map.! to)) allDeps
          maxVertex = length allNodes - 1
          graph' = buildG (0, maxVertex) edges'
      
      let depGraph = DependencyGraph
            { graph = graph'
            , vertexMap = vertexMap'
            , reverseVertexMap = reverseVertexMap'
            , edges = allDeps
            }
      
      return $ Right depGraph
    else return $ Left $ head failures

detectCircularDependencies :: DependencyGraph -> Either ResolutionError [[Text]]
detectCircularDependencies depGraph = 
  let graph' = graph depGraph
      reverseMap = reverseVertexMap depGraph
      allVertices = Map.keys reverseMap
      cycles = findCycles graph' reverseMap allVertices
  in if null cycles
    then Right []
    else Left $ CircularDependency $ concat cycles

findCycles :: Graph -> Map Int Text -> [Int] -> [[Text]]
findCycles graph' reverseMap vertices = 
  let visited = Set.empty
      recursionStack = Set.empty
  in concatMap (findCycleFromVertex graph' reverseMap visited recursionStack) vertices

findCycleFromVertex :: Graph -> Map Int Text -> Set Int -> Set Int -> Int -> [[Text]]
findCycleFromVertex graph' reverseMap visited recursionStack vertex = 
  if vertex `Set.member` visited
    then []
    else if vertex `Set.member` recursionStack
      then [[reverseMap Map.! vertex]]
      else do
        let newVisited = Set.insert vertex visited
            newRecursionStack = Set.insert vertex recursionStack
            neighbors = reachable graph' vertex
        concatMap (findCycleFromVertex graph' reverseMap newVisited newRecursionStack) neighbors

topologicalSort :: DependencyGraph -> Either ResolutionError [Text]
topologicalSort depGraph = do
  let graph' = graph depGraph
      reverseMap = reverseVertexMap depGraph
      sortedIndices = topSort graph'
      sortedNames = map (reverseMap Map.!) sortedIndices
  return $ Right sortedNames

createDependencyResolver :: ModuleLoader -> ModuleRegistry -> IO DependencyResolver
createDependencyResolver loader registry = do
  return $ DependencyResolver
    { moduleLoader = loader
    , moduleRegistry = registry
    , resolutionCache = Map.empty
    }

clearResolutionCache :: DependencyResolver -> DependencyResolver
clearResolutionCache resolver = resolver
  { resolutionCache = Map.empty
  }

updateResolutionCache :: DependencyResolver -> Text -> Set Text -> DependencyResolver
updateResolutionCache resolver moduleName deps = resolver
  { resolutionCache = Map.insert moduleName deps (resolutionCache resolver)
  }

getResolutionStatistics :: DependencyResolver -> Map Text Int
getResolutionStatistics resolver = 
  let cacheSize = Map.size (resolutionCache resolver)
      loaderStats = getModuleStatistics (moduleLoader resolver)
      registryStats = getRegistryStatistics (moduleRegistry resolver)
  in Map.union loaderStats registryStats

validateDependencyGraph :: DependencyGraph -> Either ResolutionError ()
validateDependencyGraph depGraph = do
  let vertexMap' = vertexMap depGraph
      edges' = edges depGraph
      allVertices = Map.keys vertexMap'
      edgeVertices = nub $ concatMap (\(from, to) -> [from, to]) edges'
      missingVertices = edgeVertices \\ allVertices
  
  unless (null missingVertices) $
    Left $ DependencyNotFound "" $ head missingVertices
  
  cycleResult <- detectCircularDependencies depGraph
  case cycleResult of
    Left _ -> return $ Right ()
    Right cycles -> unless (null cycles) $
      Left $ CircularDependency $ concat cycles

optimizeResolutionPlan :: ResolutionPlan -> ResolutionPlan
optimizeResolutionPlan plan = 
  let resolved = resolvedModules plan
      order = dependencyOrder plan
      optimizedOrder = optimizeDependencyOrder order
  in plan { dependencyOrder = optimizedOrder }

optimizeDependencyOrder :: [Text] -> [Text]
optimizeDependencyOrder order = 
  let grouped = groupBy (\a b -> take 1 a == take 1 b) order
      sortedGroups = map sort grouped
  in concat sortedGroups

checkConflicts :: DependencyResolver -> [Text] -> IO [Text]
checkConflicts resolver moduleNames = do
  let registry = moduleRegistry resolver
      allEntries = listModules registry
      requestedEntries = filter (\entry -> name (moduleInfo entry) `elem` moduleNames) allEntries
  
  let conflicts = concatMap (findConflicts allEntries) requestedEntries
  return $ nub conflicts

findConflicts :: [RegistryEntry] -> RegistryEntry -> [Text]
findConflicts allEntries entry = 
  let entryInfo = moduleInfo entry
      entryConflicts = conflicts entryInfo
      conflictingEntries = filter (\e -> any (`elem` provides (moduleInfo e)) entryConflicts) allEntries
  in map (name . moduleInfo) conflictingEntries

generateResolutionReport :: ResolutionPlan -> Text
generateResolutionReport plan = 
  let total = totalModules (metadata plan)
      deps = totalDependencies (metadata plan)
      order = dependencyOrder plan
      warnings = warnings plan
  in Text.unlines
    [ "Resolution Plan:"
    , "Total modules: " <> Text.pack (show total)
    , "Total dependencies: " <> Text.pack (show deps)
    , "Dependency order: " <> Text.intercalate " -> " order
    , "Warnings: " <> Text.intercalate ", " warnings
    ]
