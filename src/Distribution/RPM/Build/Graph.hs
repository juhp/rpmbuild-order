{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
This module provides simple dependency graph making for rpm packages:

@
import Distribution.RPM.Build.Graph

graph <- createGraph ["pkg1", "pkg2", "../pkg3"]
@
-}

module Distribution.RPM.Build.Graph
  (createGraph,
   createGraph',
   dependencyNodes,
   subgraph',
   packageLayers,
   lowestLayer,
   lowestLayer',
   packageLeaves,
   separatePackages,
   PackageGraph
  ) where

import qualified Data.CaseInsensitive as CI
import Data.Graph.Inductive.Query.DFS (scc, xdfsWith)
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Graph as G

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (guard, when, unless)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.List.Extra (dropSuffix, find)
import System.Directory (doesDirectoryExist, doesFileExist,
#if !MIN_VERSION_directory(1,2,5)
                         getDirectoryContents
#endif
                        )
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath
-- replace with warning
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

#if !MIN_VERSION_directory(1,2,5)
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  filter f <$> getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."
#endif

data SourcePackage =
  SourcePackage {
    packagePath :: FilePath,
    dependencies :: [FilePath]
   }
   deriving Eq

-- | alias for a package dependency graph
type PackageGraph = Gr FilePath ()

-- | Get all of the dependencies of a subset of one or more packages within full PackageGraph.
-- The subset paths should be written in the same way as for the graph.
dependencyNodes :: [FilePath] -- ^ subset of packages to start from
                -> PackageGraph -- ^ dependency graph
                -> [FilePath] -- ^ dependencies of subset
dependencyNodes subset graph =
  let nodes = G.labNodes graph
      subnodes = mapMaybe (pkgNode nodes) subset
  in xdfsWith G.pre' third subnodes graph
  where
    pkgNode :: [G.LNode FilePath] -> FilePath -> Maybe Int
    pkgNode [] _ = Nothing
    pkgNode ((i,l):ns) p = if dropSuffix "/" p == dropSuffix "/" l then Just i else pkgNode ns p

    third (_, _, c, _) = c

-- | Create a directed dependency graph for a set of packages
-- This is a convenience wrapper for createGraph' False False True Nothing
createGraph :: [FilePath] -- ^ package paths (directories or spec filepaths)
            -> IO PackageGraph -- ^ dependency graph labelled by package paths
createGraph = createGraph' False False True Nothing

-- | Create a directed dependency graph for a set of packages
-- For the (createGraph default) reverse deps graph the arrows point back
-- from the dependencies to the dependendent (parent/consumer) packages,
-- and this allows forward sorting by dependencies (ie lowest deps first).
createGraph' :: Bool -- ^ verbose
             -> Bool -- ^ lenient (skip rpmspec failures)
             -> Bool -- ^ reverse dependency graph
             -> Maybe FilePath -- ^ look for spec file in a subdirectory
             -> [FilePath] -- ^ package paths (directories or spec filepaths)
             -> IO PackageGraph -- ^ dependency graph labelled by package paths
createGraph' verbose lenient rev mdir paths = do
  metadata <- catMaybes <$> mapM readSpecMetadata paths
  let realpkgs = map fst3 metadata
      deps = mapMaybe (getDepsSrcResolved metadata) realpkgs
      spkgs = zipWith SourcePackage realpkgs deps
      graph = getBuildGraph spkgs
  checkForCycles graph
  return graph
  where
    readSpecMetadata :: FilePath -> IO (Maybe (FilePath,[String],[String]))
    readSpecMetadata path = do
      mspec <- findSpec
      case mspec of
        Nothing -> return Nothing
        Just spec -> do
          when verbose $ hPutStrLn stderr spec
          mcontent <- rpmspecParse spec
          case mcontent of
            Nothing -> return Nothing
            Just content ->
              let pkg = takeBaseName spec
                  (provs,brs) = extractMetadata pkg ([],[]) $ lines content
              in return (Just (path, provs, brs))
      where
        findSpec :: IO (Maybe FilePath)
        findSpec =
          if takeExtension path == ".spec"
            then checkFile lenient path
            else do
            dirp <- doesDirectoryExist path
            if dirp
              then do
              let dir = maybe path (path </>) mdir
                  pkg = takeFileName $ dropSuffix "/" path
              mspec <- checkFile True $ dir </> pkg ++ ".spec"
              case mspec of
                Nothing -> do
                  dead <- doesFileExist $ dir </> "dead.package"
                  return $ if dead || lenient then Nothing
                           else error $ "No spec file found in " ++ path
                Just spec -> return $ Just spec
              else error $ "No spec file found for " ++ path
          where
            checkFile :: Bool -> FilePath -> IO (Maybe FilePath)
            checkFile may f = do
              e <- doesFileExist f
              if e
                then return $ Just f
                else return $ if may
                              then Nothing
                              else error $ f ++ " not found"

        extractMetadata :: FilePath -> ([String],[String]) -> [String] -> ([String],[String])
        extractMetadata _ acc [] = acc
        extractMetadata pkg acc@(provs,brs) (l:ls) =
          let ws = words l in
            if length ws < 2 then extractMetadata pkg acc ls
            else case CI.mk (head ws) of
              "BuildRequires:" -> extractMetadata pkg (provs,(head . tail) ws : brs) ls
              "Name:" -> extractMetadata pkg ((head . tail) ws : provs, brs) ls
              "Provides:" -> extractMetadata pkg ((head . tail) ws : provs, brs) ls
              "%package" ->
                let subpkg =
                      let sub = last ws in
                        if length ws == 2
                        then pkg ++ '-' : sub
                        else sub
                in extractMetadata pkg (subpkg : provs, brs) ls
              _ -> extractMetadata pkg acc ls

    getBuildGraph :: [SourcePackage] -> PackageGraph
    getBuildGraph srcPkgs =
       let nodes = zip [0..] srcPkgs
           nodeDict = zip (map packagePath srcPkgs) [0..]
           edges = do
              (srcNode,srcPkg) <- nodes
              dstNode <- mapMaybe (`lookup` nodeDict) (dependencies srcPkg)
              guard (dstNode /= srcNode)
              return $ if rev
                       then (dstNode, srcNode, ())
                       else (srcNode, dstNode,  ())
       in G.mkGraph (map (fmap packagePath) nodes) edges

    checkForCycles :: Monad m => PackageGraph -> m ()
    checkForCycles graph =
       case getCycles graph of
          [] -> return ()
          cycles ->
            error $ unlines $
            "Cycles in dependencies:" :
            map (unwords . nodeLabels graph) cycles
      where
        getCycles :: Gr a b -> [[G.Node]]
        getCycles =
           filter ((>= 2) . length) . scc

    getDepsSrcResolved :: [(FilePath,[String],[String])] -> FilePath -> Maybe [FilePath]
    getDepsSrcResolved metadata pkg =
      map resolveBase . thd <$> find ((== pkg) . fst3) metadata
      where
        resolveBase :: FilePath -> FilePath
        resolveBase br =
          case mapMaybe (\ (p,provs,_) -> if br `elem` provs then Just p else Nothing) metadata of
            [] -> br
            [p] -> p
            ps -> error $ pkg ++ ": " ++ br ++ " is provided by: " ++ unwords ps

        thd (_,_,c) = c

    fst3 :: (a,b,c) -> a
    fst3 (a,_,_) = a

    nodeLabels :: Gr a b -> [G.Node] -> [a]
    nodeLabels graph =
       map (fromMaybe (error "node not found in graph") .
            G.lab graph)

    rpmspecParse :: FilePath -> IO (Maybe String)
    rpmspecParse spec = do
      (res, out, err) <- readProcessWithExitCode "rpmspec" ["-P", "--define", "ghc_version any", spec] ""
      unless (null err) $ hPutStrLn stderr err
      case res of
        ExitFailure _ -> if lenient then return Nothing else exitFailure
        ExitSuccess -> return $ Just out

-- | A flipped version of subgraph
subgraph' :: Gr a b -> [G.Node] -> Gr a b
subgraph' = flip G.subgraph

-- | Return the bottom-up list of dependency layers of a graph
packageLayers :: PackageGraph -> [[FilePath]]
packageLayers graph =
  if G.isEmpty graph then []
  else
    let layer = lowestLayer' graph
    in map snd layer : packageLayers (G.delNodes (map fst layer) graph)

-- | The lowest dependencies of a PackageGraph
lowestLayer :: PackageGraph -> [FilePath]
lowestLayer graph =
  map snd $ G.labNodes $ G.nfilter ((==0) . G.indeg graph) graph

-- | The lowest dependency nodes of a PackageGraph
lowestLayer' :: PackageGraph -> [G.LNode FilePath]
lowestLayer' graph =
  G.labNodes $ G.nfilter ((==0) . G.indeg graph) graph

-- | The leaf (outer) packages of a PackageGraph
packageLeaves :: PackageGraph -> [FilePath]
packageLeaves graph =
  map snd $ G.labNodes $ G.nfilter ((==0) . G.outdeg graph) graph

-- | Returns packages independent of all the rest of the graph
separatePackages :: PackageGraph -> [FilePath]
separatePackages graph =
  map snd $ G.labNodes $ G.nfilter ((==0) . G.deg graph) graph
