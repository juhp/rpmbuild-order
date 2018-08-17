{-# LANGUAGE LambdaCase #-}

module Main where

import System.Console.GetOpt
          (getOpt, ArgOrder(..), OptDescr(..), ArgDescr(..), usageInfo)
import System.Exit (exitSuccess, exitFailure)
import qualified System.Environment as Env
import System.FilePath

import System.Directory (doesDirectoryExist, doesFileExist,
#if (defined(MIN_VERSION_directory) && MIN_VERSION_directory(1,2,5))
                         listDirectory
#else
                         getDirectoryContents
#endif
  )
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)

import Data.Graph.Inductive.Query.DFS (xdfsWith, topsort', scc, components)
import Data.Graph.Inductive.Tree (Gr)
import qualified Data.Graph.Inductive.Graph as Graph

import qualified Control.Monad.Exception.Synchronous as E
import qualified Control.Monad.Trans.Class as T

import qualified Data.Set as Set
import Control.Monad (guard, when, unless)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.List (delete, intersperse, stripPrefix)
import Data.Version  (showVersion)

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
#else
import Control.Applicative ((<$>))
#endif

#if (defined(MIN_VERSION_directory) && MIN_VERSION_directory(1,2,5))
#else
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  filter f <$> getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."
#endif

import Paths_rpmbuild_order (version)

main :: IO ()
main =
  E.resolveT handleException $ do
  argv <- T.lift Env.getArgs
  let (opts, args, errors) = getOpt Permute options argv
  flags <-
    E.ExceptionalT $ return $
        foldr (=<<)
           (return
            Flags {optHelp = False,
                   optVersion = False,
                   optVerbosity = False,
                   optFormat = package,
                   optParallel = Nothing,
                   optBranch = Nothing})
           opts
  if optHelp flags
    then T.lift $ help >> exitSuccess
    else
    if optVersion flags
    then T.lift $ putStrLn $ showVersion version
    else
      if length args < 2
      then T.lift $ help >> exitFailure
      else do
        let (com:pkgs) = args
        unless (null errors) $ E.throwT $ concat errors
        unless (com `elem` ["sort", "deps", "rdeps"]) $
          E.throwT $ "Unknown command " ++ com
        runCommand flags com $ map (removeSuffix "/") pkgs
  where
    help =
      Env.getProgName >>= \programName ->
        putStrLn
             (usageInfo ("Usage: " ++ programName ++
                         " [OPTIONS] [sort|deps|rdeps] PKG-SPEC-OR-DIR ...") options)

handleException :: String -> IO ()
handleException msg = do
   putStrLn $ "Aborted: " ++ msg
   exitFailure

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

data Flags =
   Flags {
      optHelp :: Bool,
      optVersion :: Bool,
      optVerbosity :: Bool,
      optFormat :: SourcePackage -> String,
      optParallel :: Maybe String,
      optBranch :: Maybe FilePath
   }

options :: [OptDescr (Flags -> E.Exceptional String Flags)]
options =
  [
    Option ['h'] ["help"]
      (NoArg (\flags -> return $ flags{optHelp = True}))
      "Show options"
  , Option ['V'] ["version"]
      (NoArg (\flags -> return $ flags{optVersion = True}))
      "Show version"
  , Option ['p'] ["parallel"]
      (OptArg
        (\mstr flags ->
            fmap (\cs -> flags{optParallel = Just cs})
            (E.Success (fromMaybe "" mstr)))
         "SEPARATOR")
      "Display independently buildable groups of packages, optionally with separator"
  , Option ['b'] ["branch"]
      (ReqArg
         (\str flags ->
            fmap (\mb -> flags{optBranch = mb})
            (E.Success (Just str)))
         "BRANCHDIR")
    "branch directory"
  , Option ['f'] ["format"]
      (ReqArg
         (\str flags ->
            fmap (\select -> flags{optFormat = select}) $
            case str of
               "package" -> E.Success package
               "spec" -> E.Success location
               "dir"  -> E.Success (takeDirectory . location)
               _ ->
                  E.Exception $
                  "unknown info type " ++ str)
         "KIND")
      "output format: 'package' (default), 'spec', or 'dir'"
  , Option ['v'] ["verbose"]
      (NoArg
         (\flags ->
            return flags{optVerbosity = True}))
      "verbose output"
  ]

type Package = String

data SourcePackage =
   SourcePackage {
      location :: FilePath,
      package :: Package,
      dependencies :: [Package]
   }
   deriving (Show, Eq)

type Command = String

runCommand :: Flags -> Command -> [Package] -> E.ExceptionalT String IO ()
runCommand flags "sort" pkgs = sortSpecFiles flags pkgs
runCommand flags "deps" pkgs = depsSpecFiles False flags pkgs
runCommand flags "rdeps" pkgs = depsSpecFiles True flags pkgs
runCommand _ _ _ = E.throwT "impossible happened"

createGraphNodes :: Flags -> [Package] -> [Package] ->
               E.ExceptionalT String IO (Gr SourcePackage (), [Graph.Node])
createGraphNodes flags pkgs subset = do
  unless (all (`elem` pkgs) subset) $
    E.throwT "Packages must be in the current directory"
  specPaths <- T.lift $ catMaybes <$> mapM (findSpec (optBranch flags)) (filter (/= fromMaybe "" (optParallel flags)) pkgs)
  let names = map takeBaseName specPaths
  provs <-
     T.lift $
     mapM (readProvides (optVerbosity flags)) specPaths
  let resolves = zip names provs
  deps <-
     T.lift $
     mapM (getDepsSrcResolved (optVerbosity flags) resolves) specPaths
  let spkgs = zipWith3 SourcePackage specPaths names deps
      graph = getBuildGraph spkgs
  checkForCycles graph
  let nodes = Graph.labNodes graph
      subnodes = mapMaybe (pkgNode nodes) subset
  return (graph, subnodes)
  where
    pkgNode [] _ = Nothing
    pkgNode ((i,l):ns) p = if p == package l then Just i else pkgNode ns p

sortSpecFiles :: Flags -> [Package] -> E.ExceptionalT String IO ()
sortSpecFiles flags pkgs = do
      (graph, _) <- createGraphNodes flags pkgs []
      T.lift $
         case optParallel flags of
           Just s ->
             mapM_ ((putStrLn . unwords . (if null s then id else intersperse s) . map (optFormat flags)) . topsort' . subgraph graph)
                 (components graph)
           Nothing ->
             mapM_ (putStrLn . optFormat flags) $ topsort' graph
 
depsSpecFiles :: Bool -> Flags -> [Package] -> E.ExceptionalT String IO ()
depsSpecFiles rev flags pkgs = do
  allpkgs <- T.lift $ filter (\ f -> head f /= '.') <$> listDirectory "."
  (graph, nodes) <- createGraphNodes flags allpkgs pkgs
  let dir = if rev then Graph.suc' else Graph.pre'
  sortSpecFiles flags $ map package $ xdfsWith dir third nodes graph
  where
    third (_, _, c, _) = c

readProvides :: Bool -> FilePath -> IO [String]
readProvides verbose file = do
  when verbose $ hPutStrLn stderr file
  pkgs <- lines <$>
    rpmspec ["--rpms", "--qf=%{name}\n", "--define", "ghc_version any"] Nothing file
  let pkg = takeBaseName file
  return $ delete pkg pkgs

getDepsSrcResolved :: Bool -> [(String,[String])] -> FilePath -> IO [String]
getDepsSrcResolved verbose provides file =
  map (resolveBase provides) <$> do
      when verbose $ hPutStrLn stderr file
      -- ignore version bounds
      map (head . words) . lines <$>
        rpmspec ["--buildrequires", "--define", "ghc_version any"] Nothing file
  where
    resolveBase :: [(String,[String])] -> String -> String
    resolveBase provs br =
      case mapMaybe (\ (pkg,subs) -> if br `elem` subs then Just pkg else Nothing) provs of
        [] -> br
        [p] -> p
        ps -> error $ br ++ "is provided by: " ++ unwords ps

removeSuffix :: String -> String -> String
removeSuffix suffix orig =
  fromMaybe orig $ stripSuffix suffix orig
  where
    stripSuffix sf str = reverse <$> stripPrefix (reverse sf) (reverse str)

cmdStdIn :: String -> [String] -> String -> IO String
cmdStdIn c as inp = removeTrailingNewline <$> readProcess c as inp
  where
    removeTrailingNewline :: String -> String
    removeTrailingNewline "" = ""
    removeTrailingNewline str =
      if last str == '\n'
      then init str
      else str

cmd :: String -> [String] -> IO String
cmd c as = cmdStdIn c as ""

rpmspec :: [String] -> Maybe String -> FilePath -> IO String
rpmspec args mqf spec = do
  let qf = maybe [] (\ q -> ["--queryformat", q]) mqf
  cmd "rpmspec" (["-q"] ++ args ++ qf ++ [spec])

getDeps :: Gr SourcePackage () -> [(SourcePackage, [SourcePackage])]
getDeps gr =
    let c2dep :: Graph.Context SourcePackage () -> (SourcePackage, [SourcePackage])
        c2dep ctx =
           (Graph.lab' ctx,
            map (Graph.lab' . Graph.context gr) (Graph.pre gr . Graph.node' $ ctx))
    in  Graph.ufold (\ctx ds -> c2dep ctx : ds) [] gr

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

checkForCycles ::
   Monad m =>
   Gr SourcePackage () ->
   E.ExceptionalT String m ()
checkForCycles graph =
   case getCycles graph of
      [] -> return ()
      cycles ->
         E.throwT $ unlines $
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
   filter (\case
              _:_:_ -> True
              _ -> False)
   . scc
