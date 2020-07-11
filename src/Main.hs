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
import qualified Data.Graph.Inductive.Query.DFS as DFS
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

sortSpecFiles :: Bool -> Bool -> Components -> Maybe FilePath -> [Package] -> IO ()
sortSpecFiles verbose lenient components mdir pkgs = do
  graph <- createGraph verbose lenient mdir pkgs
  case components of
    Parallel ->
      mapM_ ((B.putStrLn . B.cons '\n' . B.unwords) . DFS.topsort' . subgraph graph) (DFS.components graph)
    Combine -> (B.putStrLn . B.unwords . DFS.topsort') graph
    Connected ->
      mapM_ ((B.putStrLn . B.cons '\n' . B.unwords) . DFS.topsort' . subgraph graph) $ filter ((>1) . length) (DFS.components graph)
    Separate ->
      let independent = separatePackages graph
      in mapM_ B.putStrLn independent

depsSpecFiles :: Bool -> Bool -> Bool -> Bool -> Maybe FilePath -> [Package] -> IO ()
depsSpecFiles rev verbose lenient parallel mdir pkgs = do
  allpkgs <- map B.pack . filter (\ f -> head f /= '.') <$> listDirectory "."
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

chainOrderFiles :: Bool -> Bool -> Bool -> Maybe FilePath -> [Package] -> IO ()
chainOrderFiles verbose lenient combine mdir pkgs = do
  graph <- createGraph verbose lenient mdir pkgs
  if combine then doChain graph
    else mapM_ doChain $ map (subgraph graph) (DFS.components graph)
  where
    doChain graph =
      let chain = intercalate [B.pack ":"] $ packageLayers graph
      in B.putStrLn $ B.intercalate (B.pack " ") chain

leavesFiles :: Bool -> Bool -> Maybe FilePath -> [Package] -> IO ()
leavesFiles verbose lenient mdir pkgs = do
  graph <- createGraph verbose lenient mdir pkgs
  let leaves = packageLeaves graph
  mapM_ B.putStrLn leaves

rootFiles :: Bool -> Bool -> Maybe FilePath -> [Package] -> IO ()
rootFiles verbose lenient mdir pkgs = do
  graph <- createGraph verbose lenient mdir pkgs
  let roots = map snd $ lowestLayer graph
  mapM_ B.putStrLn roots
