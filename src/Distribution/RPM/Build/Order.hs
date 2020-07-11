{-# LANGUAGE CPP #-}

module Distribution.RPM.Build.Order
  (dependencySort,
   dependencySortParallel,
   dependencyLayers,
   leafPackages,
   independentPackages)
where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Data.Graph.Inductive.Query.DFS (topsort', components)

import Distribution.RPM.Build.Graph

-- | sort packages by dependencies
dependencySort :: [String] -> IO [String]
dependencySort pkgs = do
  topsort' <$> createGraph False False Nothing pkgs

-- | dependency sort of packages in graph components
dependencySortParallel :: [String] -> IO [[String]]
dependencySortParallel pkgs = do
  graph <- createGraph False False Nothing pkgs
  return $ map (topsort' . subgraph graph) (components graph)

-- | group packages in dependency layers, lowest first
dependencyLayers :: [String] -> IO [[String]]
dependencyLayers pkgs = do
  graph <- createGraph False False Nothing pkgs
  return $ packageLayers graph

leafPackages :: [String] -> IO [String]
leafPackages pkgs = do
  graph <- createGraph False False Nothing pkgs
  return $ packageLeaves graph

independentPackages :: [String] -> IO [String]
independentPackages pkgs = do
  graph <- createGraph False False Nothing pkgs
  return $ separatePackages graph
