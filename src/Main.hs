{-# LANGUAGE CPP #-}

import Control.Applicative (
#if !MIN_VERSION_simple_cmd_args(0,1,3)
                            (<|>),
#endif
#if !MIN_VERSION_simple_cmd_args(0,1,4)
                            many, some,
#endif
#if !MIN_VERSION_base(4,8,0)
                            (<$>), (<*>)
#endif
                           )
import Control.Monad.Extra
import Data.Graph.Inductive.Query.DFS (components)
import Data.List

#if !MIN_VERSION_simple_cmd_args(0,1,4)
import Options.Applicative (str)
#endif
import SimpleCmdArgs
import System.Directory (doesDirectoryExist,
#if MIN_VERSION_directory(1,2,5)
                         listDirectory
#else
                         getDirectoryContents
#endif
  )

import Distribution.RPM.Build.Graph
import Distribution.RPM.Build.Order
import Paths_rpmbuild_order (version)

main :: IO ()
main =
  simpleCmdArgs (Just version) "Order packages by build dependencies"
  "Sort package sources (spec files) in build dependency order" $
  subcommands
  [ Subcommand "sort" "sort packages" $
    sortPackages <$> verboseOpt <*> lenientOpt <*> rpmOpts <*> componentsOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "deps" "sort dependencies" $
    depsPackages False <$> verboseOpt <*> lenientOpt <*> rpmOpts <*> combineOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "rdeps" "sort dependents" $
    depsPackages True <$> verboseOpt <*> lenientOpt <*> rpmOpts <*> combineOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "layers" "ordered output suitable for a chain-build" $
    layerPackages <$> verboseOpt <*> lenientOpt <*> rpmOpts <*> combineOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "chain" "ordered output suitable for a chain-build" $
    chainPackages <$> verboseOpt <*> lenientOpt <*> rpmOpts <*> combineOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "leaves" "List of the top leaves of package graph" $
    leavesPackages <$> verboseOpt <*> lenientOpt <*> rpmOpts <*> subdirOpt <*> pkgArgs
  , Subcommand "roots" "List lowest root packages" $
    rootPackages <$> verboseOpt <*> lenientOpt <*> rpmOpts <*> subdirOpt <*> pkgArgs
  ]
  where
    verboseOpt = switchWith 'v' "verbose" "Verbose output for debugging"
    lenientOpt = switchWith 'l' "lenient" "Ignore rpmspec errors"
    combineOpt = switchWith 'c' "combine" "Combine independent packages"
    subdirOpt = optional (strOptionWith 'd' "dir" "SUBDIR" "Branch directory")
    pkgArgs = some (argumentWith str "PKG...")
    componentsOpt =
      flagWith' Connected 'C' "connected" "Only include connected packages" <|>
      flagWith' Separate 'i' "independent" "Only list independent packages" <|>
      flagWith Parallel Combine 'c' "combine" "Combine connected and independent packages"
    rpmOpts = many (strOptionWith 'r' "rpmopt" "RPMOPT" "Option for rpmspec")

sortPackages :: Bool -> Bool -> [String] -> Components -> Maybe FilePath -> [FilePath] -> IO ()
sortPackages verbose lenient rpmopts opts mdir pkgs = do
  createGraph'' verbose lenient True rpmopts mdir pkgs >>= sortGraph opts

depsPackages :: Bool -> Bool -> Bool -> [String] -> Bool -> Maybe FilePath -> [FilePath] -> IO ()
depsPackages rev verbose lenient rpmopts parallel mdir pkgs = do
  unlessM (and <$> mapM doesDirectoryExist pkgs) $
    error "Please use package directory paths"
  listDirectory "." >>=
    -- filter out dotfiles
    createGraph'' verbose lenient (not rev) rpmopts mdir . filter ((/= '.') . head) >>=
    createGraph'' verbose lenient True rpmopts mdir . dependencyNodes pkgs >>=
    sortGraph (if parallel then Parallel else Combine)

#if (defined(MIN_VERSION_directory) && MIN_VERSION_directory(1,2,5))
#else
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  filter f <$> getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."
#endif

layerPackages :: Bool -> Bool -> [String] -> Bool -> Maybe FilePath -> [FilePath] -> IO ()
layerPackages verbose lenient rpmopts combine mdir pkgs = do
  graph <- createGraph'' verbose lenient True rpmopts mdir pkgs
  if combine then printLayers graph
    else mapM_ (printLayers . subgraph' graph) (components graph)
  where
    printLayers =  putStrLn . unlines . map unwords . packageLayers

chainPackages :: Bool -> Bool -> [String] -> Bool -> Maybe FilePath -> [FilePath] -> IO ()
chainPackages verbose lenient rpmopts combine mdir pkgs = do
  graph <- createGraph'' verbose lenient True rpmopts mdir pkgs
  if combine then doChain graph
    else mapM_ (doChain . subgraph' graph) (components graph)
  where
    doChain graph =
      let chain = intercalate [":"] $ packageLayers graph
      in putStrLn $ unwords chain

leavesPackages :: Bool -> Bool -> [String] -> Maybe FilePath -> [FilePath] -> IO ()
leavesPackages verbose lenient rpmopts mdir pkgs = do
  graph <- createGraph'' verbose lenient True rpmopts mdir pkgs
  mapM_ putStrLn $ packageLeaves graph

rootPackages :: Bool -> Bool -> [String] -> Maybe FilePath -> [FilePath] -> IO ()
rootPackages verbose lenient rpmopts mdir pkgs = do
  graph <- createGraph'' verbose lenient True rpmopts mdir pkgs
  mapM_ putStrLn $ lowestLayer graph
