{-# LANGUAGE CPP #-}

module Distribution.RPM.Build.Order
  (dependencySort,
   dependencySortParallel,
   dependencyLayers,
   leafPackages,
   independentPackages,
   Components (..),
   sortGraph)
where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Data.Graph.Inductive.Query.DFS (topsort', components)

import Distribution.RPM.Build.Graph

-- | sort packages by dependencies
dependencySort :: [FilePath] -> IO [FilePath]
dependencySort pkgs = do
  topsort' <$> createGraph pkgs

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

leafPackages :: [FilePath] -> IO [FilePath]
leafPackages pkgs = do
  graph <- createGraph pkgs
  return $ packageLeaves graph

independentPackages :: [FilePath] -> IO [FilePath]
independentPackages pkgs = do
  graph <- createGraph pkgs
  return $ separatePackages graph

data Components = Parallel | Combine | Connected | Separate

sortGraph :: Components -> PackageGraph -> IO ()
sortGraph opt graph = do
  case opt of
    Parallel ->
      mapM_ ((putStrLn . ('\n':) . unwords) . topsort' . subgraph' graph) (components graph)
    Combine -> (putStrLn . unwords . topsort') graph
    Connected ->
      mapM_ ((putStrLn . ('\n':) . unwords) . topsort' . subgraph' graph) $ filter ((>1) . length) (components graph)
    Separate ->
      let independent = separatePackages graph
      in mapM_ putStrLn independent
