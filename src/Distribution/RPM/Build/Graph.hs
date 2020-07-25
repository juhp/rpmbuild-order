{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.RPM.Build.Graph
  (createGraph,
   subGraphNodes,
   subgraph',
   packageLayers,
   lowestLayer,
   packageLeaves,
   separatePackages,
   PackageGraph
  ) where

import qualified Data.CaseInsensitive as CI
import Data.Graph.Inductive.Query.DFS (scc, xdfWith)
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Graph as G

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

type PackageGraph = Gr FilePath ()

-- | Create a directed package dependency graph with respect to a subset of packages
subGraphNodes :: [FilePath] -- ^ subset of packages to start from
              -> PackageGraph -- ^ dependency graph
              -> PackageGraph -- ^ dependency subgraph
subGraphNodes subset graph =
  let nodes = G.labNodes graph
      subnodes = mapMaybe (pkgNode nodes) subset
  in snd $ xdfWith G.suc' (const ()) subnodes graph
  where
    pkgNode [] _ = Nothing
    pkgNode ((i,l):ns) p = if p == l then Just i else pkgNode ns p

-- | Create a directed dependency graph for a set of packages
-- If rev Note the arrows of the graph actually point back
-- from the dependencies to the dependendent (parent/consumer) packages
createGraph :: Bool -- ^ verbose
            -> Bool -- ^ lenient (skip rpmspec failures)
            -> Bool -- ^ reverse dependency graph
            -> Maybe FilePath -- ^ look for spec file in a subdirectory
            -> [FilePath] -- ^ package paths (directories or spec filepaths)
            -> IO PackageGraph -- ^ dependency graph labelled by package paths
createGraph verbose lenient rev mdir paths = do
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
      mspec <- findSpec path
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
              in return $ Just (path, provs, brs)
      where
        findSpec :: FilePath -> IO (Maybe FilePath)
        findSpec file =
          if takeExtension file == ".spec"
            then checkFile lenient file
            else do
            dirp <- doesDirectoryExist file
            if dirp
              then do
              let dir = maybe file (file </>) mdir
                  pkg = takeBaseName file
              mspec <- checkFile True $ dir </> pkg ++ ".spec"
              case mspec of
                Nothing -> do
                  dead <- doesFileExist $ dir </> "dead.package"
                  return $ if dead || lenient then Nothing
                           else error $ "No spec file found in " ++ file
                Just spec -> return $ Just spec
              else error $ "No spec file found for " ++ file
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

subgraph' :: Gr a b -> [G.Node] -> Gr a b
subgraph' = flip G.subgraph

-- returns the first word for each line
rpmspecParse :: Bool -> FilePath -> IO (Maybe String)
rpmspecParse lenient spec = do
  (res, out, err) <- readProcessWithExitCode "rpmspec" ["-P", "--define", "ghc_version any", spec] ""
  unless (null err) $ hPutStrLn stderr err
  case res of
    ExitFailure _ -> if lenient then return Nothing else exitFailure
    ExitSuccess -> return $ Just out

-- | Return the list of dependency layers of a graph
packageLayers :: PackageGraph -> [[FilePath]]
packageLayers graph =
  if G.isEmpty graph then []
  else
    let layer = lowestLayer graph
    in map snd layer : packageLayers (G.delNodes (map fst layer) graph)


lowestLayer :: PackageGraph -> [G.LNode FilePath]
lowestLayer graph =
  G.labNodes $ G.nfilter ((==0) . G.indeg graph) graph

packageLeaves :: PackageGraph -> [FilePath]
packageLeaves graph =
  map snd $ G.labNodes $ G.nfilter ((==0) . G.outdeg graph) graph

-- | Returns packages independent of all the rest of the graph
separatePackages :: PackageGraph -> [FilePath]
separatePackages graph =
  map snd $ G.labNodes $ G.nfilter ((==0) . G.deg graph) graph
