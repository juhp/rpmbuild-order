module Distribution.RPM.Build.Order
  (dependencySort,
   dependencySortParallel)
where

import Data.Graph.Inductive.Query.DFS (topsort', components)

import Distribution.RPM.Build.Graph

-- | sort packages by dependencies
dependencySort :: [Package] -> IO [Package]
dependencySort pkgs = do
  (graph, _) <- createGraphNodes False False Nothing pkgs []
  return $ map package $ topsort' graph

-- | dependency sort of packages in graph components
dependencySortParallel :: [Package] -> IO [[Package]]
dependencySortParallel pkgs = do
  (graph, _) <- createGraphNodes False False Nothing pkgs []
  return $ map ((map package) . topsort' . subgraph graph) (components graph)
