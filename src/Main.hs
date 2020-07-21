{-# LANGUAGE CPP #-}

import Control.Applicative (
#if !MIN_VERSION_simple_cmd_args(0,1,3)
                            (<|>),
#endif
#if !MIN_VERSION_simple_cmd_args(0,1,4)
                            some,
#endif
#if !MIN_VERSION_base(4,8,0)
                            (<$>), (<*>)
#endif
                           )
import qualified Data.Graph.Inductive.Graph as Graph
import qualified Data.Graph.Inductive.Query.DFS as DFS
import Data.List

#if !MIN_VERSION_simple_cmd_args(0,1,4)
import Options.Applicative (str)
#endif
import SimpleCmdArgs
import System.Directory (
#if MIN_VERSION_directory(1,2,5)
                         listDirectory
#else
                         getDirectoryContents
#endif
  )

import Distribution.RPM.Build.Graph
import Paths_rpmbuild_order (version)

main :: IO ()
main =
  simpleCmdArgs (Just version) "Order packages by build dependencies"
  "Sort package sources (spec files) in build dependency order" $
  subcommands
  [ Subcommand "sort" "sort packages" $
    sortSpecFiles <$> verboseOpt <*> lenientOpt <*> componentsOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "deps" "sort dependencies" $
    depsSpecFiles False <$> verboseOpt <*> lenientOpt <*> combineOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "rdeps" "sort dependents" $
    depsSpecFiles True <$> verboseOpt <*> lenientOpt <*> combineOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "chain" "ordered output suitable for a chain-build" $
    chainOrderFiles <$> verboseOpt <*> lenientOpt <*> combineOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "leaves" "List of the top leaves of package graph" $
    leavesFiles <$> verboseOpt <*> lenientOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "roots" "List lowest root packages" $
    rootFiles <$> verboseOpt <*> lenientOpt <*> subdirOpt <*> pkgArgs
  ]
  where
    verboseOpt = switchWith 'v' "verbose" "Verbose output for debugging"
    lenientOpt = switchWith 'l' "lenient" "Ignore rpmspec errors"
    combineOpt = switchWith 'c' "combine" "Combine independent packages"
    subdirOpt = optional (strOptionWith 'd' "dir" "SUBDIR" "Branch directory")
    pkgArgs = some (argumentWith str "PKG...")
    componentsOpt =
      flagWith' Connected 'C' "connected" "Only include connected packages" <|>
      flagWith' Separate 's' "separated" "Only list independent packages" <|>
      flagWith Parallel Combine 'c' "combine" "Separate independent packages"

data Components = Parallel | Combine | Connected | Separate

sortSpecFiles :: Bool -> Bool -> Components -> Maybe FilePath -> [FilePath] -> IO ()
sortSpecFiles verbose lenient components mdir pkgs = do
  graph <- createGraph verbose lenient mdir pkgs
  case components of
    Parallel ->
      mapM_ ((putStrLn . ('\n':) . unwords) . DFS.topsort' . subgraph graph) (DFS.components graph)
    Combine -> (putStrLn . unwords . DFS.topsort') graph
    Connected ->
      mapM_ ((putStrLn . ('\n':) . unwords) . DFS.topsort' . subgraph graph) $ filter ((>1) . length) (DFS.components graph)
    Separate ->
      let independent = separatePackages graph
      in mapM_ putStrLn independent

depsSpecFiles :: Bool -> Bool -> Bool -> Bool -> Maybe FilePath -> [FilePath] -> IO ()
depsSpecFiles rev verbose lenient parallel mdir pkgs = do
  allpkgs <- filter ((/= '.') . head) <$> listDirectory "."
  (graph, nodes) <- createGraphNodes verbose lenient mdir allpkgs pkgs
  let direction = if rev then Graph.suc' else Graph.pre'
  sortSpecFiles verbose lenient (if parallel then Parallel else Combine) mdir $ DFS.xdfsWith direction third nodes graph
  where
    third (_, _, c, _) = c

#if (defined(MIN_VERSION_directory) && MIN_VERSION_directory(1,2,5))
#else
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  filter f <$> getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."
#endif

chainOrderFiles :: Bool -> Bool -> Bool -> Maybe FilePath -> [FilePath] -> IO ()
chainOrderFiles verbose lenient combine mdir pkgs = do
  graph <- createGraph verbose lenient mdir pkgs
  if combine then doChain graph
    else mapM_ (doChain . subgraph graph) (DFS.components graph)
  where
    doChain graph =
      let chain = intercalate [":"] $ packageLayers graph
      in putStrLn $ unwords chain

leavesFiles :: Bool -> Bool -> Maybe FilePath -> [FilePath] -> IO ()
leavesFiles verbose lenient mdir pkgs = do
  graph <- createGraph verbose lenient mdir pkgs
  let leaves = packageLeaves graph
  mapM_ putStrLn leaves

rootFiles :: Bool -> Bool -> Maybe FilePath -> [FilePath] -> IO ()
rootFiles verbose lenient mdir pkgs = do
  graph <- createGraph verbose lenient mdir pkgs
  let roots = map snd $ lowestLayer graph
  mapM_ putStrLn roots
