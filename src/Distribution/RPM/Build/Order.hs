module Distribution.RPM.Build.Order
  (dependencySort,
   dependencySortParallel)
where

import qualified Data.ByteString.Char8 as B
import Data.Graph.Inductive.Query.DFS (topsort', components)

import Distribution.RPM.Build.Graph

-- | sort packages by dependencies
dependencySort :: [String] -> IO [String]
dependencySort pkgs = do
  (graph, _) <- createGraphNodes False False Nothing (map B.pack pkgs) []
  return $ map (B.unpack . package) $ topsort' graph

-- | dependency sort of packages in graph components
dependencySortParallel :: [String] -> IO [[String]]
dependencySortParallel pkgs = do
  (graph, _) <- createGraphNodes False False Nothing (map B.pack pkgs) []
  return $ map ((map (B.unpack . package)) . topsort' . subgraph graph) (components graph)
