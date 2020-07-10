{-# LANGUAGE CPP #-}

module Distribution.RPM.Build.Graph
  (Package,
   SourcePackage(package),
   createGraph,
   createGraphNodes,
   subgraph,
   packageLayers
  ) where

import Data.Graph.Inductive.Query.DFS ({-xdfsWith, topsort',-} scc, {-components-})
import Data.Graph.Inductive.Tree (Gr)
import qualified Data.Graph.Inductive.Graph as Graph

import qualified Data.Set as Set
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (guard, when, unless)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.List (delete)
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

type Package = B.ByteString

data SourcePackage =
   SourcePackage {
      package :: Package,
      dependencies :: [Package]
   }
   deriving (Show, Eq)

-- | create a directed packages dependency graph together with subset of nodes
createGraphNodes :: Bool -> Bool -> Maybe FilePath -> [Package] -> [Package] ->
                    IO (Gr Package (), [Graph.Node])
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
createGraph :: Bool -> Bool -> Maybe FilePath -> [Package] -> IO (Gr Package ())
createGraph verbose lenient mdir pkgs = do
  specPaths <- catMaybes <$> mapM (findSpec . B.unpack) pkgs
  let names = map (B.pack . takeBaseName) specPaths
  resolves <- catMaybes <$> mapM readProvides specPaths
  deps <- catMaybes <$> mapM (getDepsSrcResolved verbose lenient resolves) specPaths
  let spkgs = zipWith SourcePackage names deps
      graph = getBuildGraph spkgs
  checkForCycles graph
  return graph
  where
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

    readProvides :: FilePath -> IO (Maybe (Package,[Package]))
    readProvides file = do
      when verbose $ hPutStrLn stderr file
      mpkgs <- rpmspecQuery lenient ["-q", "--provides", "--define", "ghc_version any"] file
      case mpkgs of
        Nothing -> return Nothing
        Just provs ->
          let pkg = B.pack $ takeBaseName file in
            return $ Just (pkg, delete pkg provs)

    getBuildGraph :: [SourcePackage] -> Gr Package ()
    getBuildGraph srcPkgs =
       let nodes = zip [0..] srcPkgs
           nodeDict = zip (map package srcPkgs) [0..]
           edges = do
              (srcNode,srcPkg) <- nodes
              dstNode <- mapMaybe (`lookup` nodeDict) (dependencies srcPkg)
              guard (dstNode /= srcNode)
              return (dstNode, srcNode, ())
       in Graph.mkGraph (map (fmap package) nodes) edges

    checkForCycles :: Monad m => Gr Package () -> m ()
    checkForCycles graph =
       case getCycles graph of
          [] -> return ()
          cycles ->
            error $ unlines $
            "Cycles in dependencies:" :
            map (B.unpack . B.unwords . nodeLabels graph) cycles
      where
        getCycles :: Gr a b -> [[Graph.Node]]
        getCycles =
           filter ((>= 2) . length) . scc

getDepsSrcResolved :: Bool -> Bool -> [(Package,[Package])] -> FilePath -> IO (Maybe [Package])
getDepsSrcResolved verbose lenient provides file = do
  when verbose $ hPutStrLn stderr file
  fmap (map resolveBase) <$>
    rpmspecQuery lenient ["--buildrequires", "--define", "ghc_version any"] file
  where
    resolveBase :: Package -> Package
    resolveBase br =
      case mapMaybe (\ (pkg,subs) -> if br `elem` subs then Just pkg else Nothing) provides of
        [] -> br
        [p] -> p
        ps -> error $ B.unpack br ++ " is provided by: " ++ (B.unpack . B.unwords) ps

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
rpmspecQuery :: Bool -> [String] -> FilePath -> IO (Maybe [B.ByteString])
rpmspecQuery lenient args spec = do
  (res, out, err) <- readProcessWithExitCode "rpmspec" (["-q"] ++ args ++ [spec]) ""
  unless (null err) $ hPutStrLn stderr err
  case res of
    ExitFailure _ -> if lenient then return Nothing else exitFailure
    ExitSuccess -> return $ Just $ map takeFirst $ B.lines (B.pack out)
  where
    -- ignore version bounds
    takeFirst :: B.ByteString -> B.ByteString
    takeFirst = head . B.words

packageLayers :: Gr Package () -> [[Package]]
packageLayers graph =
  if Graph.isEmpty graph then []
  else
    let layer = lowestLayer graph
    in [map snd layer] ++ packageLayers (Graph.delNodes (map fst layer) graph)

lowestLayer :: Gr Package () -> [Graph.LNode Package]
lowestLayer graph =
  Graph.labNodes $ Graph.nfilter ((==0) . (Graph.indeg graph)) graph
