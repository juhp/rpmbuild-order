{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.RPM.Build.Graph
  (createGraph,
   createGraphNodes,
   subgraph,
   packageLayers,
   lowestLayer,
   packageLeaves,
   separatePackages
  ) where

import qualified Data.CaseInsensitive as CI
import Data.Graph.Inductive.Query.DFS ({-xdfsWith, topsort',-} scc, {-components-})
import Data.Graph.Inductive.Tree (Gr)
import qualified Data.Graph.Inductive.Graph as Graph

import qualified Data.Set as Set
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (guard, when, unless)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.List
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

-- | create a directed packages dependency graph together with subset of nodes
createGraphNodes :: Bool -> Bool -> Maybe FilePath -> [FilePath] -> [FilePath] ->
                    IO (Gr FilePath (), [Graph.Node])
createGraphNodes verbose lenient mdir pkgs subset = do
  unless (all (`elem` pkgs) subset) $
    error "Packages must be in the current directory"
  graph <- createGraph verbose lenient mdir pkgs
  let nodes = Graph.labNodes graph
      subnodes = mapMaybe (pkgNode nodes) subset
  return (graph, subnodes)
  where
    pkgNode [] _ = Nothing
    pkgNode ((i,l):ns) p = if p == l then Just i else pkgNode ns p

-- | create a directed packages dependency graph
createGraph :: Bool -> Bool -> Maybe FilePath -> [FilePath] -> IO (Gr FilePath ())
createGraph verbose lenient mdir pkgdirs = do
  metadata <- catMaybes <$> mapM readSpecMetadata pkgdirs
  let deps = mapMaybe (getDepsSrcResolved metadata) pkgdirs
      spkgs = zipWith SourcePackage pkgdirs deps
      graph = getBuildGraph spkgs
  checkForCycles graph
  return graph
  where
    -- FIXME seems too lenient for non-existent spec file
    -- FIXME ignore dead.package instead?
    findSpec :: FilePath -> IO (Maybe FilePath)
    findSpec file =
      if takeExtension file == ".spec"
        then checkFile file
        else do
        dirp <- doesDirectoryExist file
        if dirp
          then
          let dir = maybe file (file </>) mdir
              pkg = takeBaseName file in
            checkFile $ dir </> pkg ++ ".spec"
          else error $ "No spec file found for " ++ file
      where
        checkFile :: FilePath -> IO (Maybe FilePath)
        checkFile f = do
          e <- doesFileExist f
          if e
            then return $ Just f
            else return Nothing

    readSpecMetadata :: FilePath -> IO (Maybe (FilePath,[String],[String]))
    readSpecMetadata dir = do
      mspec <- findSpec dir
      case mspec of
        Nothing -> return Nothing
        Just spec -> do
          when verbose $ hPutStrLn stderr spec
          mcontent <- rpmspecParse lenient spec
          case mcontent of
            Nothing -> return Nothing
            Just content ->
              let pkg = takeBaseName spec
                  (provs,brs) = extractMetadata pkg ([],[]) $ lines content
              in return $ Just (dir, provs, brs)
      where
        extractMetadata :: FilePath -> ([String],[String]) -> [String] -> ([String],[String])
        extractMetadata _ acc [] = acc
        extractMetadata pkg acc@(provs,brs) (l:ls) =
          let ws = words l in
            if length ws < 2 then extractMetadata pkg acc ls
            else case CI.mk (head ws) of
              "BuildRequires:" -> extractMetadata pkg (provs,(head . tail) ws : brs) ls
              "Provides:" -> extractMetadata pkg ((head . tail) ws : provs, brs) ls
              "%package" ->
                let subpkg =
                      let sub = last ws in
                        if length ws == 2
                        then pkg ++ '-' : sub
                        else sub
                in extractMetadata pkg (subpkg : provs, brs) ls
              _ -> extractMetadata pkg acc ls

    getBuildGraph :: [SourcePackage] -> Gr FilePath ()
    getBuildGraph srcPkgs =
       let nodes = zip [0..] srcPkgs
           nodeDict = zip (map packagePath srcPkgs) [0..]
           edges = do
              (srcNode,srcPkg) <- nodes
              dstNode <- mapMaybe (`lookup` nodeDict) (dependencies srcPkg)
              guard (dstNode /= srcNode)
              return (dstNode, srcNode, ())
       in Graph.mkGraph (map (fmap packagePath) nodes) edges

    checkForCycles :: Monad m => Gr FilePath () -> m ()
    checkForCycles graph =
       case getCycles graph of
          [] -> return ()
          cycles ->
            error $ unlines $
            "Cycles in dependencies:" :
            map (unwords . nodeLabels graph) cycles
      where
        getCycles :: Gr a b -> [[Graph.Node]]
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

    fst3 (a,_,_) = a
    thd (_,_,c) = c

nodeLabels :: Gr a b -> [Graph.Node] -> [a]
nodeLabels graph =
   map (fromMaybe (error "node not found in graph") .
        Graph.lab graph)

subgraph :: Gr a b -> [Graph.Node] -> Gr a b
subgraph graph nodes =
   let nodeSet = Set.fromList nodes
       edges = do
           from <- nodes
           (to, lab) <- Graph.lsuc graph from
           guard $ Set.member from nodeSet && Set.member to nodeSet
           return (from,to,lab)
   in  Graph.mkGraph (zip nodes $ nodeLabels graph nodes) edges

-- returns the first word for each line
rpmspecParse :: Bool -> FilePath -> IO (Maybe String)
rpmspecParse lenient spec = do
  (res, out, err) <- readProcessWithExitCode "rpmspec" ["-P", "--define", "ghc_version any", spec] ""
  unless (null err) $ hPutStrLn stderr err
  case res of
    ExitFailure _ -> if lenient then return Nothing else exitFailure
    ExitSuccess -> return $ Just out

packageLayers :: Gr FilePath () -> [[FilePath]]
packageLayers graph =
  if Graph.isEmpty graph then []
  else
    let layer = lowestLayer graph
    in map snd layer : packageLayers (Graph.delNodes (map fst layer) graph)

lowestLayer :: Gr FilePath () -> [Graph.LNode FilePath]
lowestLayer graph =
  Graph.labNodes $ Graph.nfilter ((==0) . Graph.indeg graph) graph

packageLeaves :: Gr FilePath () -> [FilePath]
packageLeaves graph =
  map snd $ Graph.labNodes $ Graph.nfilter ((==0) . Graph.outdeg graph) graph

separatePackages :: Gr FilePath () -> [FilePath]
separatePackages graph =
  map snd $ Graph.labNodes $ Graph.nfilter ((==0) . Graph.deg graph) graph
