{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
This module provides simple dependency graph making for rpm packages:

@
import "Distribution.RPM.Build.Graph"

graph <- 'createGraph' ["pkg1", "pkg2", "../pkg3"]
@
-}

module Distribution.RPM.Build.Graph
  (PackageGraph,
   createGraph,
   createGraphRpmOpts,
   createGraph1,
   createGraph2,
   createGraph3,
   createGraph4,
   createGraph',
   createGraph'',
   createGraph''',
   createGraph'''',
   dependencyNodes,
   subgraph',
   packageLayers,
   lowestLayer,
   lowestLayer',
   packageLeaves,
   separatePackages,
   printGraph,
   renderGraph
  ) where

import qualified Data.CaseInsensitive as CI
import Data.Graph.Inductive.Query.DFS (scc, xdfsWith)
import Data.Graph.Inductive.Query.SP (sp)
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Graph as G

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM_, guard, when, unless)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.List.Extra
import Data.GraphViz
import SimpleCmd
import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, withCurrentDirectory,
#if !MIN_VERSION_simple_cmd(0,2,4)
#if MIN_VERSION_directory(1,2,5)
                         listDirectory
#else
                         getDirectoryContents
#endif
#endif
                        )
import System.Exit (exitFailure)
import System.FilePath
import System.IO.Extra (withTempDir)

#if !MIN_VERSION_directory(1,2,5) && !MIN_VERSION_simple_cmd(0,2,4)
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  filter f <$> getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."
#endif

data SourcePackage =
  SourcePackage {
    packagePath :: FilePath,
    dependencies :: [FilePath]
   }
   deriving Eq

-- | alias for a package dependency graph
type PackageGraph = Gr FilePath ()

-- | Get all of the dependencies of a subset of one or more packages within full PackageGraph.
-- The subset paths should be written in the same way as for the graph.
dependencyNodes :: [FilePath] -- ^ subset of packages to start from
                -> PackageGraph -- ^ dependency graph
                -> [FilePath] -- ^ dependencies of subset
dependencyNodes subset graph =
  let nodes = G.labNodes graph
      subnodes = mapMaybe (pkgNode nodes) subset
  in xdfsWith G.pre' third subnodes graph
  where
    pkgNode :: [G.LNode FilePath] -> FilePath -> Maybe Int
    pkgNode [] _ = Nothing
    pkgNode ((i,l):ns) p = if dropSuffix "/" p == dropSuffix "/" l then Just i else pkgNode ns p

    third (_, _, c, _) = c

-- | Create a directed dependency graph for a set of packages
-- This is a convenience wrapper for createGraph1 False False True Nothing
createGraph :: [FilePath] -- ^ package paths (directories or spec filepaths)
            -> IO PackageGraph -- ^ dependency graph labelled by package paths
createGraph = createGraph1 False False True Nothing

-- | Create a directed dependency graph for a set of packages setting rpm options
-- This is a convenience wrapper for @createGraph2 rpmopts False False True Nothing@
--
-- @since 0.4.2
createGraphRpmOpts :: [String] -- ^ rpmspec options
                   -> [FilePath] -- ^ package paths (directories or spec filepaths)
                   -> IO PackageGraph -- ^ dependency graph labelled by package paths
createGraphRpmOpts rpmopts =
  createGraph2 rpmopts False False True Nothing

-- | Create a directed dependency graph for a set of packages
-- For the (createGraph default) reverse deps graph the arrows point back
-- from the dependencies to the dependendent (parent/consumer) packages,
-- and this allows forward sorting by dependencies (ie lowest deps first).
--
-- This is the same as @createGraph2 []@
--
-- @since 0.4.6
createGraph1 :: Bool -- ^ verbose
             -> Bool -- ^ lenient (skip rpmspec failures)
             -> Bool -- ^ reverse dependency graph
             -> Maybe FilePath -- ^ look for spec file in a subdirectory
             -> [FilePath] -- ^ package paths (directories or spec filepaths)
             -> IO PackageGraph -- ^ dependency graph labelled by package paths
createGraph1 = createGraph2 []

-- | Alias for createGraph1
createGraph' :: Bool -- ^ verbose
             -> Bool -- ^ lenient (skip rpmspec failures)
             -> Bool -- ^ reverse dependency graph
             -> Maybe FilePath -- ^ look for spec file in a subdirectory
             -> [FilePath] -- ^ package paths (directories or spec filepaths)
             -> IO PackageGraph -- ^ dependency graph labelled by package paths
createGraph' = createGraph1

-- | Create a directed dependency graph for a set of packages
-- For the (createGraph default) reverse deps graph the arrows point back
-- from the dependencies to the dependendent (parent/consumer) packages,
-- and this allows forward sorting by dependencies (ie lowest deps first).
--
-- Additionally this function allows passing options to rpmspec:
-- eg `--with bootstrap` etc
--
-- @since 0.4.6
createGraph2 :: [String] -- ^ rpmspec options
             -> Bool -- ^ verbose
             -> Bool -- ^ lenient (skip rpmspec failures)
             -> Bool -- ^ reverse dependency graph
             -> Maybe FilePath -- ^ look for spec file in a subdirectory
             -> [FilePath] -- ^ package paths (directories or spec filepaths)
             -> IO PackageGraph -- ^ dependency graph labelled by package paths
createGraph2 = createGraph3 []

-- | Alias for createGraph2
--
-- @since 0.4.2 (deprecated)
createGraph'' :: [String] -- ^ rpmspec options
              -> Bool -- ^ verbose
              -> Bool -- ^ lenient (skip rpmspec failures)
              -> Bool -- ^ reverse dependency graph
              -> Maybe FilePath -- ^ look for spec file in a subdirectory
              -> [FilePath] -- ^ package paths (directories or spec filepaths)
              -> IO PackageGraph -- ^ dependency graph labelled by package paths
createGraph'' = createGraph2

-- | Create a directed dependency graph for a set of packages
--
-- Like createGraph2 but with additional parameter for any BRs to be ignored.
--
-- @since 0.4.6
createGraph3 :: [String] -- ^ ignored BuildRequires
             -> [String] -- ^ rpmspec options
             -> Bool -- ^ verbose
             -> Bool -- ^ lenient (skip rpmspec failures)
             -> Bool -- ^ reverse dependency graph
             -> Maybe FilePath -- ^ look for spec file in a subdirectory
             -> [FilePath] -- ^ package paths (directories or spec filepaths)
             -> IO PackageGraph -- ^ dependency graph labelled by package paths
createGraph3 = createGraph4 True

-- | Alias for createGraph3
--
-- @since 0.4.3 (deprecated)
createGraph''' :: [String] -- ^ ignored BuildRequires
               -> [String] -- ^ rpmspec options
               -> Bool -- ^ verbose
               -> Bool -- ^ lenient (skip rpmspec failures)
               -> Bool -- ^ reverse dependency graph
               -> Maybe FilePath -- ^ look for spec file in a subdirectory
               -> [FilePath] -- ^ package paths (directories or spec filepaths)
               -> IO PackageGraph -- ^ dependency graph labelled by package paths
createGraph''' = createGraph3

-- | Create a directed dependency graph for a set of packages
--
-- Like createGraph3 but can disable check for cycles
--
-- @since 0.4.6
createGraph4 :: Bool -- ^ check for cycles
             -> [String] -- ^ ignored BuildRequires
             -> [String] -- ^ rpmspec options
             -> Bool -- ^ verbose
             -> Bool -- ^ lenient (skip rpmspec failures)
             -> Bool -- ^ reverse dependency graph
             -> Maybe FilePath -- ^ look for spec file in a subdirectory
             -> [FilePath] -- ^ package paths (directories or spec filepaths)
             -> IO PackageGraph -- ^ dependency graph labelled by package paths
createGraph4 checkcycles ignoredBRs rpmopts verbose lenient rev mdir paths =
  do
  metadata <- catMaybes <$> mapM readSpecMetadata paths
  let realpkgs = map fst3 metadata
      deps = mapMaybe (getDepsSrcResolved metadata) realpkgs
      spkgs = zipWith SourcePackage realpkgs deps
      graph = getBuildGraph spkgs
  when checkcycles $
    checkForCycles graph
  return graph
  where
    readSpecMetadata :: FilePath -> IO (Maybe (FilePath,[String],[String]))
    readSpecMetadata path = do
      mspecdir <- findSpec
      case mspecdir of
        Nothing -> return Nothing
        Just (dir,spec) -> do
          when verbose $ warning spec
          withCurrentDirectory dir $ do
            dynbr <- egrep_ "^\\(%generate_buildrequires\\|%gometa\\)" spec
            mprovbrs <-
              if dynbr
              then do
                brs <- rpmspecDynBuildRequires spec
                provs <- do
                  dynprovs <-
                    if "golang-" `isPrefixOf` takeBaseName spec
                    then do
                      macro <- grep "%global goipath" spec
                      return $
                        case macro of
                          [def] -> ["golang(" ++ last (words def) ++ ")"]
                          _ -> error' $ "failed to find %goipath in " ++ spec
                    else return []
                  prs <- rpmspecProvides spec
                  return $ dynprovs ++ prs
                return $ Just (provs,brs)
              else do
                mcontent <- rpmspecParse spec
                return $ case mcontent of
                           Nothing -> Nothing
                           Just content ->
                             let pkg = takeBaseName spec
                             in Just $ extractMetadata pkg ([],[]) $ lines content
            case mprovbrs of
              Nothing -> return Nothing
              Just (provs,brs) -> do
                when verbose $ do
                  warning $ show $ sort provs
                  warning $ show $ mapMaybe simplifyDep $ sort brs
                return $ Just (path,
                               nub provs, -- FIXME filter 'name(arch)'
                               nub (mapMaybe simplifyDep brs) \\ ignoredBRs)
      where
        -- (dir,specfile)
        findSpec :: IO (Maybe (FilePath,FilePath))
        findSpec =
          fmap splitFileName <$>
          if takeExtension path == ".spec"
            then checkFile lenient path
            else do
            dirp <- doesDirectoryExist path
            if dirp
              then do
              let dir = maybe path (path </>) mdir
                  dirname = takeFileName $ dropSuffix "/" path
              mspec <- checkFile True $ dir </> dirname ++ ".spec"
              case mspec of
                Just spec -> return $ Just spec
                Nothing -> do
                  dead <- doesFileExist $ dir </> "dead.package"
                  if dead then return Nothing
                    else do
                    specs <- filesWithExtension dir ".spec"
                    case specs of
                      [spec] -> return $ Just $ dir </> spec
                      _ -> if lenient then return Nothing
                           else error' $
                                if null specs
                                then "No spec file found in " ++ path
                                else "More than one .spec file found in " ++ dir
              else error' $ "No spec file found for " ++ path
          where
            checkFile :: Bool -> FilePath -> IO (Maybe FilePath)
            checkFile may f = do
              e <- doesFileExist f
              return $ if e
                       then Just f
                       else if may
                            then Nothing
                            else error' $ f ++ " not found"

#if !MIN_VERSION_simple_cmd(0,2,4)
            filesWithExtension :: FilePath -> String -> IO [FilePath]
            filesWithExtension dir ext =
              filter (ext `isExtensionOf`) <$> listDirectory dir
#endif

        extractMetadata :: FilePath -> ([String],[String]) -> [String] -> ([String],[String])
        extractMetadata _ acc [] = acc
        extractMetadata pkg acc@(provs,brs) (l:ls) =
          let ws = words l in
            if length ws < 2 then extractMetadata pkg acc ls
            else case CI.mk (head ws) of
              "BuildRequires:" ->
                let br = (head . tail) ws
                    brs' = if br `elem` ignoredBRs then brs else br:brs
                in extractMetadata pkg (provs, brs') ls
              "Name:" -> extractMetadata pkg ((head . tail) ws : provs, brs) ls
              "Provides:" -> extractMetadata pkg ((head . tail) ws : provs, brs) ls
              "%package" ->
                let subpkg =
                      let sub = last ws in
                        if length ws == 2
                        then pkg ++ '-' : sub
                        else sub
                in extractMetadata pkg (subpkg : provs, brs) ls
              _ -> extractMetadata pkg acc ls

    getBuildGraph :: [SourcePackage] -> PackageGraph
    getBuildGraph srcPkgs =
       let nodes = zip [0..] srcPkgs
           nodeDict = zip (map packagePath srcPkgs) [0..]
           edges = do
              (srcNode,srcPkg) <- nodes
              dstNode <- mapMaybe (`lookup` nodeDict) (dependencies srcPkg)
              guard (dstNode /= srcNode)
              return $ if rev
                       then (dstNode, srcNode, ())
                       else (srcNode, dstNode,  ())
       in G.mkGraph (map (fmap packagePath) nodes) $ nub edges

    checkForCycles :: PackageGraph -> IO ()
    checkForCycles graph = do
      let cycles = filter ((>= 2) . length) (scc graph)
      unless (null cycles) $ do
        let plural = if length cycles > 1 then "s" else ""
        error' $ unlines $
          ("ordering not possible due to build dependency cycle" ++ plural ++ ":\n") : intercalate [""] (map (renderCycles . subcycles) cycles)
      where
        -- shortest subcycle
        subcycles :: [G.Node] -> ([FilePath],[[FilePath]])
        subcycles [] = error "cyclic graph with no nodes!"
        subcycles cycle' =
          let shorter = nubOrdOn sort $ sortOn length $ filter ((< length cycle') . length) $ mapMaybe findSp $ G.labEdges sg
          in (nodeLabels graph cycle', map (nodeLabels sg) shorter)
          where
            sg = G.emap (const (1 :: Int)) $ G.subgraph cycle' graph

            findSp (i,j,_) | G.hasEdge sg (i,j) = sp j i sg
                           | G.hasEdge sg (j,i) = sp i j sg
                           | otherwise = Nothing

        renderCycles :: ([FilePath],[[FilePath]]) -> [String]
        renderCycles (c,sc) =
          unwords c : if null sc then [] else "\nShortest path subcycles: " : map unwords sc

    getDepsSrcResolved :: [(FilePath,[String],[String])] -> FilePath -> Maybe [FilePath]
    getDepsSrcResolved metadata pkg =
      map resolveBase . thd <$> find ((== pkg) . fst3) metadata
      where
        resolveBase :: FilePath -> FilePath
        resolveBase br =
          case mapMaybe (\ (p,provs,_) -> if br `elem` provs then Just p else Nothing) metadata of
            [] -> br
            [p] -> p
            ps -> error' $ pkg ++ ": " ++ br ++ " is provided by: " ++ unwords ps

        thd (_,_,c) = c

    fst3 :: (a,b,c) -> a
    fst3 (a,_,_) = a

    nodeLabels :: Gr a b -> [G.Node] -> [a]
    nodeLabels graph =
       map (fromMaybe (error "node not found in graph") .
            G.lab graph)

    rpmspecParse :: FilePath -> IO (Maybe String)
    rpmspecParse spec = do
      (ok, out, err) <- cmdFull "rpmspec" (["-P", "--define", "ghc_version any"] ++ rpmopts ++ [spec]) ""
      unless (null err) $ warning err
      if ok
        then return $ Just out
        else if lenient then return Nothing else exitFailure

    rpmspecProvides :: FilePath -> IO [String]
    rpmspecProvides spec = do
      (ok, out, err) <- cmdFull "rpmspec" (["--define", "ghc_version any", "-q", "--provides"] ++ rpmopts ++ [spec]) ""
      unless (null err) $ warning err
      if ok
        then return $ map (head . words) $ lines out
        else if lenient then return [] else exitFailure

    rpmspecDynBuildRequires :: FilePath -> IO [String]
    rpmspecDynBuildRequires spec = do
      cwd <- getCurrentDirectory
      withTempDir $ \tmpdir -> do
        (out,err) <- cmdStdErr "rpmbuild" ["-br", "--nodeps", "--define", "_srcrpmdir " ++ tmpdir, cwd </> spec]
        unless (null err) $ when verbose $ warning err
        -- Wrote: /current/dir/SRPMS/name-version-release.buildreqs.nosrc.rpm
        cmdLines "rpm" ["-qp", "--requires", last (words out)]

    simplifyDep br =
      case (head . words) br of
        '(':dep -> simplifyDep dep
        dep -> case splitOn "(" (dropSuffix ")" dep) of
          ("rpmlib":_) -> Nothing
          ("crate":[crate]) -> Just $ "rust-" ++ replace "/" "+" crate ++ "-devel"
          ("rubygem":[gem]) -> Just $ "rubygem-" ++ gem
          _ -> Just dep

    -- rpmspecBuildRequires :: FilePath -> IO [String]
    -- rpmspecBuildRequires spec = do
    --   (ok, out, err) <- cmdFull "rpmspec" (["--define", "ghc_version any", "-q", "--buildrequires"] ++ rpmopts ++ [spec]) ""
    --   unless (null err) $ warning err
    --   if ok
    --     then return $ lines out
    --     else if lenient then return [] else exitFailure

-- | Alias for createGraph4
--
-- @since 0.4.4 (deprecated)
createGraph'''' :: Bool -- ^ check for cycles
                -> [String] -- ^ ignored BuildRequires
                -> [String] -- ^ rpmspec options
                -> Bool -- ^ verbose
                -> Bool -- ^ lenient (skip rpmspec failures)
                -> Bool -- ^ reverse dependency graph
                -> Maybe FilePath -- ^ look for spec file in a subdirectory
                -> [FilePath] -- ^ package paths (directories or spec filepaths)
                -> IO PackageGraph -- ^ dependency graph labelled by package paths
createGraph'''' = createGraph4

-- | A flipped version of subgraph
subgraph' :: Gr a b -> [G.Node] -> Gr a b
subgraph' = flip G.subgraph

-- | Return the bottom-up list of dependency layers of a graph
packageLayers :: PackageGraph -> [[FilePath]]
packageLayers graph =
  if G.isEmpty graph then []
  else
    let layer = lowestLayer' graph
    in map snd layer : packageLayers (G.delNodes (map fst layer) graph)

-- | The lowest dependencies of a PackageGraph
lowestLayer :: PackageGraph -> [FilePath]
lowestLayer graph =
  map snd $ G.labNodes $ G.nfilter ((==0) . G.indeg graph) graph

-- | The lowest dependency nodes of a PackageGraph
lowestLayer' :: PackageGraph -> [G.LNode FilePath]
lowestLayer' graph =
  G.labNodes $ G.nfilter ((==0) . G.indeg graph) graph

-- | The leaf (outer) packages of a PackageGraph
packageLeaves :: PackageGraph -> [FilePath]
packageLeaves graph =
  map snd $ G.labNodes $ G.nfilter ((==0) . G.outdeg graph) graph

-- | Returns packages independent of all the rest of the graph
separatePackages :: PackageGraph -> [FilePath]
separatePackages graph =
  map snd $ G.labNodes $ G.nfilter ((==0) . G.deg graph) graph

-- | Return graphviz dot format of graph
printGraph :: PackageGraph -> IO ()
printGraph g = do
  putStrLn "digraph {"
  forM_ (G.labNodes g) $ \ (n,l) -> do
    putStr $ show l
    putStrLn $ renderDeps $ map show $ mapMaybe (G.lab g . fst) $ G.lsuc g n
  putStrLn "}"
  where
    renderDeps :: [String] -> String
    renderDeps [] = ""
    renderDeps [d] = " -> " ++ d
    renderDeps ds = " -> {" ++ unwords ds ++ "}"

-- | Render graph with graphviz X11 preview
renderGraph :: PackageGraph -> IO ()
renderGraph graph = do
  gv <- isGraphvizInstalled
  if gv
    then do
    let g = G.emap (const ("" :: String)) graph
    runGraphvizCanvas' (setDirectedness graphToDot quickParams g) Xlib
    else error' "please install graphviz first"
