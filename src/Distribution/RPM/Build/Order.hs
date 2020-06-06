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
  map B.unpack . topsort' <$> createGraph False False Nothing (map B.pack pkgs)

-- | dependency sort of packages in graph components
dependencySortParallel :: [String] -> IO [[String]]
dependencySortParallel pkgs = do
  graph <- createGraph False False Nothing (map B.pack pkgs)
  return $ map (map B.unpack . topsort' . subgraph graph) (components graph)
