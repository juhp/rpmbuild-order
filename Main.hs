import System.FilePath

import System.Directory (doesDirectoryExist, doesFileExist,
#if (defined(MIN_VERSION_directory) && MIN_VERSION_directory(1,2,5))
                         listDirectory
#else
                         getDirectoryContents
#endif
  )
-- replace with warning
import System.IO (hPutStrLn, stderr)

import Data.Graph.Inductive.Query.DFS (xdfsWith, topsort', scc, components)
import Data.Graph.Inductive.Tree (Gr)
import qualified Data.Graph.Inductive.Graph as Graph

import qualified Data.Set as Set
import Control.Applicative (some,
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
                            (<$>)
#endif
                           )
import Control.Monad (guard, when, unless)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.List (delete)
import Options.Applicative (str)

import SimpleCmd.Rpm (rpmspec)
import SimpleCmdArgs
import Paths_rpmbuild_order (version)

#if (defined(MIN_VERSION_directory) && MIN_VERSION_directory(1,2,5))
#else
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  filter f <$> getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."
#endif

main :: IO ()
main =
  simpleCmdArgs (Just version) "Order packages by build dependencies"
  "Sort package sources (spec files) in build dependency order" $
  subcommands
  [ Subcommand "sort" "sort packages" $
    sortSpecFiles <$> verboseOpt <*> parallelOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "deps" "sort dependencies" $
    depsSpecFiles False <$> verboseOpt <*> parallelOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "rdeps" "sort dependents" $
    depsSpecFiles True <$> verboseOpt <*> parallelOpt <*> subdirOpt <*> pkgArgs
  ]
  where
    verboseOpt = switchWith 'v' "verbose" "Verbose output for debugging"
    parallelOpt = switchWith 'p' "parallel" "Separate independent packages"
    subdirOpt = optional (strOptionWith 'd' "dir" "SUBDIR" "Branch directory")
    pkgArgs = some (argumentWith str "PKG...")

findSpec :: Maybe FilePath -> FilePath -> IO (Maybe FilePath)
findSpec mdir file =
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

type Package = B.ByteString

data SourcePackage =
   SourcePackage {
      location :: FilePath,
      package :: Package,
      dependencies :: [Package]
   }
   deriving (Show, Eq)

createGraphNodes :: Bool -> Maybe FilePath -> [Package] -> [Package] ->
                    IO (Gr SourcePackage (), [Graph.Node])
createGraphNodes verbose mdir pkgs subset = do
  unless (all (`elem` pkgs) subset) $
    error "Packages must be in the current directory"
  specPaths <- catMaybes <$> mapM (findSpec mdir . B.unpack) pkgs
  let names = map (B.pack . takeBaseName) specPaths
  resolves <- mapM (readProvides verbose) specPaths
  deps <- mapM (getDepsSrcResolved verbose resolves) specPaths
  let spkgs = zipWith3 SourcePackage specPaths names deps
      graph = getBuildGraph spkgs
  checkForCycles graph
  let nodes = Graph.labNodes graph
      subnodes = mapMaybe (pkgNode nodes) subset
  return (graph, subnodes)
  where
    pkgNode [] _ = Nothing
    pkgNode ((i,l):ns) p = if p == package l then Just i else pkgNode ns p

sortSpecFiles :: Bool -> Bool -> Maybe FilePath -> [Package] -> IO ()
sortSpecFiles verbose parallel mdir pkgs = do
      (graph, _) <- createGraphNodes verbose mdir pkgs []
      if parallel then
        mapM_ ((B.putStrLn . B.cons '\n' . B.unwords . map package) . topsort' . subgraph graph) (components graph)
        else mapM_ (B.putStrLn . package) $ topsort' graph
 
depsSpecFiles :: Bool -> Bool -> Bool -> Maybe FilePath -> [Package] -> IO ()
depsSpecFiles rev verbose parallel mdir pkgs = do
  allpkgs <- map B.pack . filter (\ f -> head f /= '.') <$> listDirectory "."
  (graph, nodes) <- createGraphNodes verbose mdir allpkgs pkgs
  let direction = if rev then Graph.suc' else Graph.pre'
  sortSpecFiles verbose parallel mdir $ map package $ xdfsWith direction third nodes graph
  where
    third (_, _, c, _) = c

readProvides :: Bool -> FilePath -> IO (Package,[Package])
readProvides verbose file = do
  when verbose $ hPutStrLn stderr file
  pkgs <- map (B.pack . head . words) <$> rpmspec ["-q", "--provides", "--define", "ghc_version any"] Nothing file
  let pkg = B.pack $ takeBaseName file
  return (pkg, delete pkg pkgs)

getDepsSrcResolved :: Bool -> [(Package,[Package])] -> FilePath -> IO [Package]
getDepsSrcResolved verbose provides file =
  map (resolveBase provides) <$> do
      when verbose $ hPutStrLn stderr file
      -- ignore version bounds
      map (B.pack . head . words) <$>
        rpmspec ["--buildrequires", "--define", "ghc_version any"] Nothing file
  where
    resolveBase :: [(Package,[Package])] -> Package -> Package
    resolveBase provs br =
      case mapMaybe (\ (pkg,subs) -> if br `elem` subs then Just pkg else Nothing) provs of
        [] -> br
        [p] -> p
        ps -> error $ B.unpack br ++ " is provided by: " ++ (B.unpack . B.unwords) ps

getBuildGraph :: [SourcePackage] -> Gr SourcePackage ()
getBuildGraph srcPkgs =
   let nodes = zip [0..] srcPkgs
       nodeDict = zip (map package srcPkgs) [0..]
       edges = do
          (srcNode,srcPkg) <- nodes
          dstNode <- mapMaybe (`lookup` nodeDict) (dependencies srcPkg)
          guard (dstNode /= srcNode)
          return (dstNode, srcNode, ())
   in Graph.mkGraph nodes edges

checkForCycles :: Monad m => Gr SourcePackage () -> m ()
checkForCycles graph =
   case getCycles graph of
      [] -> return ()
      cycles ->
        error $ unlines $
        "Cycles in dependencies:" :
        map (unwords . map location . nodeLabels graph) cycles

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

getCycles :: Gr a b -> [[Graph.Node]]
getCycles =
   filter (\ x -> length x == 3) . scc
