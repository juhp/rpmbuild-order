{-# LANGUAGE OverloadedStrings #-}

{-|
This module has commands for reading the Requires and Provides
from an RPM package spec file.
-}

module Distribution.RPM.Build.ProvReqs
  (rpmspecProvidesBuildRequires)
where

import Control.Monad (unless)
import qualified Data.CaseInsensitive as CI
import Data.List.Extra
import Data.Maybe (mapMaybe)
import SimpleCmd (cmdFull, cmdLines, cmdStdErr, egrep_, error',
                  grep, warning, (+-+))
import SimpleCmd.Git (isGitDir)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Exit (exitFailure)
import System.FilePath
import System.IO.Extra (withTempDir)
import Text.Regex.TDFA ((=~))

generateBuildRequires :: FilePath -> IO Bool
generateBuildRequires =
  egrep_ "^\\(%generate_buildrequires\\|%gometa\\)"

-- | Get RPM Provides and BuildRequires based on spec file.
rpmspecProvidesBuildRequires :: Bool -- ^ lenient (allow failure)
                             -> [String] -- ^ RPM opts
                             -> FilePath -- ^ spec file
                             -- ghc 8.10 haddock cannot annotate inside type
                             -> IO (Maybe ([String], [String])) -- ^ (Provs,BRs)
rpmspecProvidesBuildRequires lenient rpmopts spec = do
  dynbr <- generateBuildRequires spec
  if dynbr
    then do
    brs <- rpmspecDynBuildRequires spec
    provs <- do
      dynprovs <- dynProvides
      prs <- rpmspecProvides lenient rpmopts spec
      return $ dynprovs ++ prs
    return $ Just (provs, mapMaybe simplifyDep brs)
    else do
    mcontent <- rpmspecParse
    case mcontent of
      Nothing -> return Nothing
      Just content ->
        let pkg = takeBaseName spec
        in fmap Just <$> extractMetadata pkg ([],[]) $ lines content
  where
    extractMetadata :: FilePath -> ([String],[String]) -> [String]
                    -> IO ([String],[String])
    extractMetadata _ (provs,brs) [] =
      return (provs, mapMaybe simplifyDep brs)
    extractMetadata pkg acc@(provs,brs) (l:ls) =
      case words l of
        [] -> extractMetadata pkg acc ls
        [w]
          | w =~ ("^/usr/(lib(64)?|share)/pkgconfig/.*\\.pc" :: String) ->
              let pc = metaName "pkgconfig" $ takeBaseName w
              in extractMetadata pkg (pc : provs, brs) ls
          | w =~ ("^/usr/(lib(64)?|share)/cmake/[^/]*$" :: String) ->
              let p = takeFileName w
                  cm = map (metaName "cmake") $
                       if lower p == p then [p] else [p, lower p]
              in extractMetadata pkg (provs ++ cm, brs) ls
          | otherwise -> extractMetadata pkg acc ls
        (w:w':ws) ->
            case CI.mk w of
              "BuildRequires:" ->
                -- FIXME could be more than one package: parse ws
                extractMetadata pkg (provs, w':brs) ls
              "Name:" -> extractMetadata pkg (w' : provs, brs) ls
              "Provides:" -> extractMetadata pkg (w' : provs, brs) ls
              "%package" ->
                let subpkg =
                      if null ws
                      then pkg ++ '-' : w'
                      else last ws
                in extractMetadata pkg (subpkg : provs, brs) ls
              _ -> extractMetadata pkg acc ls

    rpmspecParse :: IO (Maybe String)
    rpmspecParse = do
      (ok, out, err) <- cmdFull "rpmspec" (["-P"] ++ rpmopts ++ [spec]) ""
      unless (null err) $ warning $ spec +-+ err
      if ok
        then return $ Just out
        else if lenient then return Nothing else exitFailure

    dynProvides :: IO [String]
    dynProvides =
      if "golang-" `isPrefixOf` takeBaseName spec
      then do
        macro <- grep "%global goipath" spec
        return $
          case macro of
            [def] -> ["golang(" ++ last (words def) ++ ")"]
            _ -> error' $ "failed to find %goipath in" +-+ spec
      else return []

    simplifyDep br =
      case (head . words) br of
        '(':dep -> simplifyDep dep
        dep -> case splitOn "(" (dropSuffix ")" dep) of
          ("rpmlib":_) -> Nothing
          ("crate":[crate]) -> Just $ "rust-" ++ replace "/" "+" crate ++ "-devel"
          ("rubygem":[gem]) -> Just $ "rubygem-" ++ gem
          _ -> Just dep

rpmspecDynBuildRequires :: FilePath -> IO [String]
rpmspecDynBuildRequires spec =
  withTempDir $ \tmpdir -> do
  sourceopt <- do
    isgit <- isGitDir "."
    if isgit
      then do
      cwd <- getCurrentDirectory
      return ["--define", "_sourcedir" +-+ cwd]
      else return []
  (out,err) <- cmdStdErr "rpmbuild" $ ["-br", "--nodeps", "--define", "_srcrpmdir" +-+ tmpdir, spec] ++ sourceopt
  -- Wrote: /current/dir/SRPMS/name-version-release.buildreqs.nosrc.rpm
  let errmsg =
        "failed to generate srpm for dynamic buildrequires for" +-+ spec ++
        "\n\n" ++ err
  case words out of
    [] -> error' errmsg
    ws -> do
      let srpm = last ws
      exists <- doesFileExist srpm
      if exists
        then cmdLines "rpm" ["-qp", "--requires", last ws]
        else error' errmsg

rpmspecProvides :: Bool -> [String] -> FilePath -> IO [String]
rpmspecProvides lenient rpmopts spec = do
  (ok, out, err) <- cmdFull "rpmspec" (["-q", "--provides"] ++ rpmopts ++ [spec]) ""
  unless (null err) $ warning err
  if ok
    then return $ map (head . words) $ lines out
    else if lenient then return [] else exitFailure

metaName :: String -> String -> String
metaName meta name =
  meta ++ '(' : name ++ ")"
