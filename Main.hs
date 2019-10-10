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
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.List (delete)
import Options.Applicative (str)
import System.Directory (doesDirectoryExist, doesFileExist,
#if (defined(MIN_VERSION_directory) && MIN_VERSION_directory(1,2,5))
                         listDirectory
#else
                         getDirectoryContents
#endif
  )
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath
-- replace with warning
import System.IO (hPutStrLn, stderr)
import System.Process.Typed (proc, readProcess)

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
    sortSpecFiles <$> verboseOpt <*> lenientOpt <*> parallelOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "deps" "sort dependencies" $
    depsSpecFiles False <$> verboseOpt <*> lenientOpt <*> parallelOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "rdeps" "sort dependents" $
    depsSpecFiles True <$> verboseOpt <*> lenientOpt <*> parallelOpt <*> subdirOpt <*> pkgArgs
  ]
  where
    verboseOpt = switchWith 'v' "verbose" "Verbose output for debugging"
    lenientOpt = switchWith 'l' "lenient" "Ignore rpmspec errors"
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

createGraphNodes :: Bool -> Bool -> Maybe FilePath -> [Package] -> [Package] ->
                    IO (Gr SourcePackage (), [Graph.Node])
createGraphNodes verbose lenient mdir pkgs subset = do
  unless (all (`elem` pkgs) subset) $
    error "Packages must be in the current directory"
  specPaths <- catMaybes <$> mapM (findSpec mdir . B.unpack) pkgs
  let names = map (B.pack . takeBaseName) specPaths
  resolves <- catMaybes <$> mapM (readProvides verbose lenient) specPaths
  deps <- catMaybes <$> mapM (getDepsSrcResolved verbose lenient resolves) specPaths
  let spkgs = zipWith3 SourcePackage specPaths names deps
      graph = getBuildGraph spkgs
  checkForCycles graph
  let nodes = Graph.labNodes graph
      subnodes = mapMaybe (pkgNode nodes) subset
  return (graph, subnodes)
  where
    pkgNode [] _ = Nothing
    pkgNode ((i,l):ns) p = if p == package l then Just i else pkgNode ns p

sortSpecFiles :: Bool -> Bool -> Bool -> Maybe FilePath -> [Package] -> IO ()
sortSpecFiles verbose lenient parallel mdir pkgs = do
      (graph, _) <- createGraphNodes verbose lenient mdir pkgs []
      if parallel then
        mapM_ ((B.putStrLn . B.cons '\n' . B.unwords . map package) . topsort' . subgraph graph) (components graph)
        else mapM_ (B.putStrLn . package) $ topsort' graph
 
depsSpecFiles :: Bool -> Bool -> Bool -> Bool -> Maybe FilePath -> [Package] -> IO ()
depsSpecFiles rev verbose lenient parallel mdir pkgs = do
  allpkgs <- map B.pack . filter (\ f -> head f /= '.') <$> listDirectory "."
  (graph, nodes) <- createGraphNodes verbose lenient mdir allpkgs pkgs
  let direction = if rev then Graph.suc' else Graph.pre'
  sortSpecFiles verbose lenient parallel mdir $ map package $ xdfsWith direction third nodes graph
  where
    third (_, _, c, _) = c

readProvides :: Bool -> Bool -> FilePath -> IO (Maybe (Package,[Package]))
readProvides verbose lenient file = do
  when verbose $ hPutStrLn stderr file
  mpkgs <- rpmspecQuery lenient ["-q", "--provides", "--define", "ghc_version any"] file
  case mpkgs of
    Nothing -> return Nothing
    Just pkgs ->
      let pkg = B.pack $ takeBaseName file in
        return $ Just (pkg, delete pkg pkgs)

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

-- returns the first word for each line
rpmspecQuery :: Bool -> [String] -> FilePath -> IO (Maybe [B.ByteString])
rpmspecQuery lenient args spec = do
  let cmd = proc "rpmspec" (["-q"] ++ args ++ [spec])
  (res, out, err) <- readProcess cmd
  unless (BL.null err) $ BL.hPutStrLn stderr err
  case res of
    ExitFailure _ -> if lenient then return Nothing else exitFailure
    ExitSuccess -> return $ Just $ map (BL.toStrict . takeFirst) $ BL.lines out
  where
    -- ignore version bounds
    takeFirst :: BL.ByteString -> BL.ByteString
    takeFirst = head . BL.words
