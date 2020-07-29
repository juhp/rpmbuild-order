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
    sortPackages <$> verboseOpt <*> lenientOpt <*> componentsOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "deps" "sort dependencies" $
    depsPackages False <$> verboseOpt <*> lenientOpt <*> combineOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "rdeps" "sort dependents" $
    depsPackages True <$> verboseOpt <*> lenientOpt <*> combineOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "layers" "ordered output suitable for a chain-build" $
    layerPackages <$> verboseOpt <*> lenientOpt <*> combineOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "chain" "ordered output suitable for a chain-build" $
    chainPackages <$> verboseOpt <*> lenientOpt <*> combineOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "leaves" "List of the top leaves of package graph" $
    leavesPackages <$> verboseOpt <*> lenientOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "roots" "List lowest root packages" $
    rootPackages <$> verboseOpt <*> lenientOpt <*> subdirOpt <*> pkgArgs
  ]
  where
    verboseOpt = switchWith 'v' "verbose" "Verbose output for debugging"
    lenientOpt = switchWith 'l' "lenient" "Ignore rpmspec errors"
    combineOpt = switchWith 'c' "combine" "Combine independent packages"
    subdirOpt = optional (strOptionWith 'd' "dir" "SUBDIR" "Branch directory")
    pkgArgs = map (dropSuffix "/") <$> some (argumentWith str "PKG...")
    componentsOpt =
      flagWith' Connected 'C' "connected" "Only include connected packages" <|>
      flagWith' Separate 'i' "independent" "Only list independent packages" <|>
      flagWith Parallel Combine 'c' "combine" "Combine connected and independent packages"

sortPackages :: Bool -> Bool -> Components -> Maybe FilePath -> [FilePath] -> IO ()
sortPackages verbose lenient opts mdir pkgs = do
  createGraph' verbose lenient True mdir pkgs >>= sortGraph opts

depsPackages :: Bool -> Bool -> Bool -> Bool -> Maybe FilePath -> [FilePath] -> IO ()
depsPackages rev verbose lenient parallel mdir pkgs = do
  unlessM (and <$> mapM doesDirectoryExist pkgs) $
    error "Please use package directory paths"
  listDirectory "." >>=
    -- filter out dotfiles
    createGraph' verbose lenient (not rev) mdir . filter ((/= '.') . head) >>=
    createGraph' verbose lenient True mdir . dependencyNodes pkgs >>=
    sortGraph (if parallel then Parallel else Combine)

#if (defined(MIN_VERSION_directory) && MIN_VERSION_directory(1,2,5))
#else
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  filter f <$> getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."
#endif

layerPackages :: Bool -> Bool -> Bool -> Maybe FilePath -> [FilePath] -> IO ()
layerPackages verbose lenient combine mdir pkgs = do
  graph <- createGraph' verbose lenient True mdir pkgs
  if combine then printLayers graph
    else mapM_ (printLayers . subgraph' graph) (components graph)
  where
    printLayers =  putStrLn . unlines . map unwords . packageLayers

chainPackages :: Bool -> Bool -> Bool -> Maybe FilePath -> [FilePath] -> IO ()
chainPackages verbose lenient combine mdir pkgs = do
  graph <- createGraph' verbose lenient True mdir pkgs
  if combine then doChain graph
    else mapM_ (doChain . subgraph' graph) (components graph)
  where
    doChain graph =
      let chain = intercalate [":"] $ packageLayers graph
      in putStrLn $ unwords chain

leavesPackages :: Bool -> Bool -> Maybe FilePath -> [FilePath] -> IO ()
leavesPackages verbose lenient mdir pkgs = do
  graph <- createGraph' verbose lenient True mdir pkgs
  let leaves = packageLeaves graph
  mapM_ putStrLn leaves

rootPackages :: Bool -> Bool -> Maybe FilePath -> [FilePath] -> IO ()
rootPackages verbose lenient mdir pkgs = do
  graph <- createGraph' verbose lenient True mdir pkgs
  let roots = map snd $ lowestLayer graph
  mapM_ putStrLn roots
