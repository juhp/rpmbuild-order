{-# LANGUAGE CPP #-}

module Distribution.RPM.Build.Order
  (dependencySort,
   dependencySortParallel,
   dependencyLayers,
   leafPackages)
where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
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

-- | group packages in dependency layers, lowest first
dependencyLayers :: [String] -> IO [[String]]
dependencyLayers pkgs = do
  graph <- createGraph False False Nothing (map B.pack pkgs)
  return $ fmap (map B.unpack) $ packageLayers graph

leafPackages :: [String] -> IO [String]
leafPackages pkgs = do
  graph <- createGraph False False Nothing (map B.pack pkgs)
  return $ map B.unpack $ packageLeaves graph
