{-# LANGUAGE CPP #-}

{-|
This module provides dependency sorting functions

@
import "Distribution.RPM.Build.Order"

'dependencySort' ["pkg1", "pkg2", "../pkg3"]

=> ["pkg2", "../pkg3", "pkg1"]
@
where pkg1 depends on pkg3, which depends on pkg2 say.

Package paths can be directories or spec files.
-}

module Distribution.RPM.Build.Order
  (dependencySort,
   dependencySortRpmOpts,
   dependencySortParallel,
   dependencyLayers,
   leafPackages,
   independentPackages,
   Components (..),
   sortGraph,
   depsPackages)
where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Data.List (intercalate)
import Data.Graph.Inductive.Query.DFS (topsort', components)

import Distribution.RPM.Build.Graph

-- | sort packages by dependencies
dependencySort :: [FilePath] -> IO [FilePath]
dependencySort = dependencySortRpmOpts []

-- | sort packages by dependencies with rpm options
--
-- @since 0.4.2
dependencySortRpmOpts :: [String] -> [FilePath] -> IO [FilePath]
dependencySortRpmOpts rpmopts pkgs = do
  topsort' <$> createGraphRpmOpts rpmopts pkgs

-- | dependency sort of packages in graph components
dependencySortParallel :: [FilePath] -> IO [[FilePath]]
dependencySortParallel pkgs = do
  graph <- createGraph pkgs
  return $ map (topsort' . subgraph' graph) (components graph)

-- | group packages in dependency layers, lowest first
dependencyLayers :: [FilePath] -> IO [[FilePath]]
dependencyLayers pkgs = do
  graph <- createGraph pkgs
  return $ packageLayers graph

-- | returns the leaves of a set of packages
leafPackages :: [FilePath] -> IO [FilePath]
leafPackages pkgs = do
  graph <- createGraph pkgs
  return $ packageLeaves graph

-- | returns independent packages among a set of packages
independentPackages :: [FilePath] -> IO [FilePath]
independentPackages pkgs = do
  graph <- createGraph pkgs
  return $ separatePackages graph

-- | Used to control the output from sortGraph
data Components = Parallel -- ^ separate independent stacks
                | Combine -- ^ combine indepdendent stacks together
                | Connected -- ^ only stack of pacakges
                | Separate -- ^ only independent packages in the package set

-- | output sorted packages from a PackageGraph arrange by Components
sortGraph :: Components -> PackageGraph -> IO ()
sortGraph opt graph =
  -- FIXME output list(s) instead
  putStrLn $
  case opt of
    Parallel ->
      intercalate "\n\n" $ map (unwords . topsort' . subgraph' graph) (components graph)
    Combine -> (unwords . topsort') graph
    Connected ->
      intercalate "\n\n" $ map (unwords . topsort' . subgraph' graph) $ filter ((>1) . length) (components graph)
    Separate -> unlines $ separatePackages graph

depsPackages :: Bool -> [String] -> Bool-> [String] -> [String] -> Bool ->  Bool -> Maybe FilePath -> [FilePath] -> IO ()
depsPackages rev rpmopts verbose excludedPkgs ignoredBRs lenient parallel mdir pkgs =
  depsGraph rev rpmopts verbose excludedPkgs ignoredBRs lenient mdir pkgs >>=
  sortGraph (if parallel then Parallel else Combine)
