{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Distribution.Verbosity as Verbosity
import qualified Distribution.ReadE as ReadE

import System.Console.GetOpt
          (getOpt, ArgOrder(..), OptDescr(..), ArgDescr(..), usageInfo, )
import System.Exit (exitSuccess, exitFailure, )
import qualified System.Environment as Env
import System.FilePath

import System.Directory (doesDirectoryExist, doesFileExist)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)

import Data.Graph.Inductive.Query.DFS (topsort', scc, components, )
import Data.Graph.Inductive.Tree (Gr, )
import qualified Data.Graph.Inductive.Graph as Graph

import qualified Control.Monad.Exception.Synchronous as E
import qualified Control.Monad.Trans.Class as T

import qualified Data.Set as Set
import Control.Monad (guard, when, unless)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (delete, intersperse, stripPrefix)

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
#else
import Control.Applicative ((<$>))
#endif

main :: IO ()
main =
   E.resolveT handleException $ do
      argv <- T.lift Env.getArgs
      let (opts, pkgs, errors) =
             getOpt RequireOrder options argv
      unless (null errors) $ E.throwT $ concat errors
      flags <-
         E.ExceptionalT $ return $
            foldr (=<<)
               (return
                Flags {optHelp = False,
                       optVerbosity = Verbosity.silent,
                       optFormat = package,
                       optParallel = Nothing,
                       optBranch = Nothing})
               opts
      when (optHelp flags)
         (T.lift $
          Env.getProgName >>= \programName ->
          putStrLn
             (usageInfo ("Usage: " ++ programName ++
                         " [OPTIONS] PKG-SPEC-OR-DIR ...") options) >>
          exitSuccess)

      T.lift (mapM (findSpec (optBranch flags)) pkgs)
        >>= sortSpecFiles flags

handleException :: String -> IO ()
handleException msg = do
   putStrLn $ "Aborted: " ++ msg
   exitFailure

findSpec :: Maybe FilePath -> FilePath -> IO FilePath
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
      else error $ "Not spec file or directory: " ++ file
  where
    checkFile :: FilePath -> IO FilePath
    checkFile f = do
      e <- doesFileExist f
      if e
        then return f
        else error $ f ++ " not found"

data Flags =
   Flags {
      optHelp :: Bool,
      optVerbosity :: Verbosity.Verbosity,
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
      (ReqArg
         (\str flags ->
            fmap (\n -> flags{optVerbosity = n}) $
            E.fromEither $
            ReadE.runReadE Verbosity.flagToVerbosity str)
         "N")
      "verbosity level: 0..3"
  ]

data SourcePackage =
   SourcePackage {
      location :: FilePath,
      package :: String,
      dependencies :: [String]
   }
   deriving (Show, Eq)

sortSpecFiles :: Flags -> [FilePath] -> E.ExceptionalT String IO ()
sortSpecFiles flags specPaths = do
      let names = map takeBaseName specPaths
      provs <-
         T.lift $
         mapM (readProvides (optVerbosity flags)) specPaths
      let resolves = zip names provs
      deps <-
         T.lift $
         mapM (getDepsSrcResolved (optVerbosity flags) resolves) specPaths
      let pkgs = zipWith3 SourcePackage specPaths names deps
          graph = getBuildGraph pkgs
      checkForCycles graph
      T.lift $
         case optParallel flags of
           Just s ->
             mapM_ ((putStrLn . unwords . (if null s then id else intersperse s) . map (optFormat flags)) .
                         topsort' . subgraph graph)
                 (components graph)
           Nothing ->
             mapM_ (putStrLn . optFormat flags) $ topsort' graph
 
readProvides :: Verbosity.Verbosity -> FilePath -> IO [String]
readProvides verbose file = do
  when (verbose >= Verbosity.verbose) $ hPutStrLn stderr file
  pkgs <- lines <$>
    rpmspec ["--rpms", "--qf=%{name}\n", "--define", "ghc_version any"] Nothing file
  let pkg = takeBaseName file
  return $ delete pkg pkgs

readDependencies :: Verbosity.Verbosity -> FilePath -> IO [String]
readDependencies verbose file = do
  when (verbose >= Verbosity.verbose) $ hPutStrLn stderr file
  lines <$>
    rpmspec ["--buildrequires", "--define", "ghc_version any"] Nothing file

getDepsSrcResolved :: Verbosity.Verbosity -> [(String,[String])] -> FilePath -> IO [String]
getDepsSrcResolved verbose provides file =
  map (resolveBase provides) <$> readDependencies verbose file

resolveBase :: [(String,[String])] -> String -> String
resolveBase provs br =
  case mapMaybe (\ (pkg,subs) -> if br `elem` subs then Just pkg else Nothing) provs of
    [] -> br
    [p] -> p
    _ -> error $ "More than one package provides " ++ br

removeSuffix :: String -> String -> String
removeSuffix suffix orig =
  fromMaybe orig $ stripSuffix suffix orig
  where
    stripSuffix sf str = reverse <$> stripPrefix (reverse sf) (reverse str)


cmdStdIn :: String -> [String] -> String -> IO String
cmdStdIn c as inp = removeTrailingNewline <$> readProcess c as inp

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

getBuildGraph ::
   [SourcePackage] ->
   Gr SourcePackage ()
getBuildGraph srcPkgs =
   let nodes = zip [0..] srcPkgs
       nodeDict =
          zip
             (map package srcPkgs)
             [0..]
       edges = do
          (srcNode,srcPkg) <- nodes
          dstNode <-
             mapMaybe (`lookup` nodeDict) (dependencies srcPkg)
          guard (dstNode /= srcNode)
          return (dstNode, srcNode, ())
   in  Graph.mkGraph nodes edges


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
