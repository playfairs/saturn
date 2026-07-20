{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mycfg.Config.Validator (
    validateConfig,
    ValidationResult (..),
    ValidationWarning (..),
    ValidationError (..),
) where

import Control.Monad (unless, when)
import Data.List (nub, (\\))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Path (Abs, File, Path, parent, toFilePath)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (isPathSeparator, takeDirectory)

import Mycfg.Config.Types
import Mycfg.Errors.Types

data ValidationResult
    = ValidationSuccess [ValidationWarning]
    | ValidationFailure [ValidationError]
    deriving (Show, Eq)

data ValidationWarning
    = DeprecatedField Text Text
    | OptionalDependencyMissing Text
    | PathOutsideHome Text
    | DuplicateModule Text
    | UnusedModule Text
    deriving (Show, Eq)

data ValidationError
    = InvalidPathSyntax Text
    | CircularDependency [Text]
    | MissingRequiredField Text
    | InvalidProfileName Text
    | ConflictingFiles [Text]
    | ModuleNotFound Text
    | InvalidServiceConfig Text
    deriving (Show, Eq)

validateConfig :: Config -> IO ValidationResult
validateConfig config = do
    let errors = validateConfigStructure config
        warnings = validateConfigWarnings config

    if null errors
        then do
            pathErrors <- validatePaths config
            dependencyErrors <- validateDependencies config
            let allErrors = errors ++ pathErrors ++ dependencyErrors
            if null allErrors
                then return $ ValidationSuccess warnings
                else return $ ValidationFailure allErrors
        else return $ ValidationFailure errors

validateConfigStructure :: Config -> [ValidationError]
validateConfigStructure config =
    let profileErrors = concatMap validateProfile (Map.toList (profiles config))
        serviceErrors = concatMap validateService (Map.toList (services config))
     in profileErrors ++ serviceErrors

validateProfile :: (Text, ProfileConfig) -> [ValidationError]
validateProfile (profileName, profile) =
    let nameErrors =
            if Text.null (name profile)
                then [InvalidProfileName profileName]
                else []
        moduleErrors =
            if null (modules profile)
                then [MissingRequiredField $ "modules in profile " <> profileName]
                else []
     in nameErrors ++ moduleErrors

validateService :: (Text, ServiceConfig) -> [ValidationError]
validateService (serviceName, service) =
    let configErrors =
            if Map.null (config service)
                then []
                else []
     in configErrors

validateConfigWarnings :: Config -> [ValidationWarning]
validateConfigWarnings config =
    let moduleDuplicates = findDuplicateModules (modules config)
        unusedModules = findUnusedModules config
     in moduleDuplicates ++ unusedModules

findDuplicateModules :: [Text] -> [ValidationWarning]
findDuplicateModules modules =
    let duplicates = modules \\ nub modules
     in map DuplicateModule (nub duplicates)

findUnusedModules :: Config -> [ValidationWarning]
findUnusedModules config =
    let declaredModules = Set.fromList (modules config)
        usedInProfiles = Set.unions $ map Set.fromList $ concatMap modules (Map.elems (profiles config))
        unused = Set.toList (Set.difference declaredModules usedInProfiles)
     in map UnusedModule unused

validatePaths :: Config -> IO [ValidationError]
validatePaths config = do
    let fileTargets = Map.keys (files config)
        fileSources = Map.elems (files config)
        allPaths = fileTargets ++ fileSources
    pathValidationErrors <- mapM validateSinglePath allPaths
    return $ concat pathValidationErrors

validateSinglePath :: Text -> IO [ValidationError]
validateSinglePath path =
    if Text.null path
        then return [InvalidPathSyntax path]
        else do
            let pathStr = Text.unpack path
            if any (not . isValidPathChar) pathStr
                then return [InvalidPathSyntax path]
                else return []

isValidPathChar :: Char -> Bool
isValidPathChar c =
    not (c `elem` ['\0', '\n', '\r', '<', '>', '|', '"', '?', '*'])
        && not (isPathSeparator c && c == '/')

validateDependencies :: Config -> IO [ValidationError]
validateDependencies config = do
    let declaredModules = Set.fromList (modules config)
        profileDeps = concatMap extractProfileDependencies (Map.toList (profiles config))
        allDependencies = Set.fromList profileDeps
        missingDeps = Set.toList (Set.difference allDependencies declaredModules)

    let missingErrors = map ModuleNotFound missingDeps
        circularErrors = detectCircularDependencies config

    return $ missingErrors ++ circularErrors

extractProfileDependencies :: (Text, ProfileConfig) -> [Text]
extractProfileDependencies (_, profile) = modules profile

detectCircularDependencies :: Config -> [ValidationError]
detectCircularDependencies config =
    let moduleGraph = buildModuleGraph config
        cycles = findCycles moduleGraph
     in map CircularDependency cycles

buildModuleGraph :: Config -> Map Text [Text]
buildModuleGraph config =
    let declaredModules = modules config
        profileModules = concatMap modules (Map.elems (profiles config))
     in Map.fromList $ map (\m -> (m, [])) (declaredModules ++ profileModules)

findCycles :: Map Text [Text] -> [[Text]]
findCycles graph =
    let visited = Set.empty
        recursionStack = Set.empty
     in [] -- Simplified cycle detection - would need full implementation in practice
