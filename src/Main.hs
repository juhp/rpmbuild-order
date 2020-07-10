{-# LANGUAGE CPP #-}

import Control.Applicative (
#if (defined(MIN_VERSION_simple_cmd_args) && MIN_VERSION_simple_cmd_args(0,1,4))
#else
                            some,
#endif
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
                            (<$>), (<*>)
#endif
                           )
import qualified Data.ByteString.Char8 as B
import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.Query.DFS (xdfsWith, topsort', components)
import Data.List

#if (defined(MIN_VERSION_simple_cmd_args) && MIN_VERSION_simple_cmd_args(0,1,4))
#else
import Options.Applicative (str)
#endif
import SimpleCmdArgs
import System.Directory (
#if (defined(MIN_VERSION_directory) && MIN_VERSION_directory(1,2,5))
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
    sortSpecFiles <$> verboseOpt <*> lenientOpt <*> parallelOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "deps" "sort dependencies" $
    depsSpecFiles False <$> verboseOpt <*> lenientOpt <*> parallelOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "rdeps" "sort dependents" $
    depsSpecFiles True <$> verboseOpt <*> lenientOpt <*> parallelOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "chain" "ordered output suitable for a chain-build" $
    chainOrderFiles <$> verboseOpt <*> lenientOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "leaves" "List of the top leaves of package graph" $
    leavesFiles <$> verboseOpt <*> lenientOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "separate" "List independent packages from graph" $
    separateFiles <$> verboseOpt <*> lenientOpt <*> subdirOpt <*> pkgArgs
  ]
  where
    verboseOpt = switchWith 'v' "verbose" "Verbose output for debugging"
    lenientOpt = switchWith 'l' "lenient" "Ignore rpmspec errors"
    parallelOpt = switchWith 'p' "parallel" "Separate independent packages"
    subdirOpt = optional (strOptionWith 'd' "dir" "SUBDIR" "Branch directory")
    pkgArgs = some (argumentWith str "PKG...")

sortSpecFiles :: Bool -> Bool -> Bool -> Maybe FilePath -> [Package] -> IO ()
sortSpecFiles verbose lenient parallel mdir pkgs = do
  graph <- createGraph verbose lenient mdir pkgs
  if parallel then
    mapM_ ((B.putStrLn . B.cons '\n' . B.unwords) . topsort' . subgraph graph) (components graph)
    else mapM_ B.putStrLn $ topsort' graph

depsSpecFiles :: Bool -> Bool -> Bool -> Bool -> Maybe FilePath -> [Package] -> IO ()
depsSpecFiles rev verbose lenient parallel mdir pkgs = do
  allpkgs <- map B.pack . filter (\ f -> head f /= '.') <$> listDirectory "."
  (graph, nodes) <- createGraphNodes verbose lenient mdir allpkgs pkgs
  let direction = if rev then Graph.suc' else Graph.pre'
  sortSpecFiles verbose lenient parallel mdir $ xdfsWith direction third nodes graph
  where
    third (_, _, c, _) = c

#if (defined(MIN_VERSION_directory) && MIN_VERSION_directory(1,2,5))
#else
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  filter f <$> getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."
#endif

-- FIXME option to separate graph components
chainOrderFiles :: Bool -> Bool -> Maybe FilePath -> [Package] -> IO ()
chainOrderFiles verbose lenient mdir pkgs = do
  graph <- createGraph verbose lenient mdir pkgs
  let chain = intercalate [B.pack ":"] $ packageLayers graph
  B.putStrLn $ B.intercalate (B.pack " ") chain

leavesFiles :: Bool -> Bool -> Maybe FilePath -> [Package] -> IO ()
leavesFiles verbose lenient mdir pkgs = do
  graph <- createGraph verbose lenient mdir pkgs
  let leaves = packageLeaves graph
  mapM_ B.putStrLn leaves

separateFiles :: Bool -> Bool -> Maybe FilePath -> [Package] -> IO ()
separateFiles verbose lenient mdir pkgs = do
  graph <- createGraph verbose lenient mdir pkgs
  let independent = separatePackages graph
  mapM_ B.putStrLn independent
