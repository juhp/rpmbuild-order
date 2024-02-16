{-# LANGUAGE OverloadedStrings #-}

module Distribution.RPM.Build.ProvReqs
  (rpmspecBuildRequires,
   rpmspecDynBuildRequires,
   rpmspecProvidesBuildRequires)
where

import Control.Monad (unless)
import qualified Data.CaseInsensitive as CI
import Data.List.Extra
import SimpleCmd (cmdFull, cmdLines, cmdStdErr, egrep_, error', grep, warning,
                  (+-+))
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.FilePath
import System.IO.Extra (withTempDir)

generateBuildRequires :: FilePath -> IO Bool
generateBuildRequires =
  egrep_ "^\\(%generate_buildrequires\\|%gometa\\)"

rpmspecBuildRequires :: [String] -> FilePath -> IO [String]
rpmspecBuildRequires rpmopts spec = do
  dynbr <- generateBuildRequires spec
  if dynbr
    then rpmspecDynBuildRequires spec
    else cmdLines "rpmspec" (["-q", "--buildrequires"] ++ rpmopts ++ [spec])

rpmspecProvidesBuildRequires :: Bool -> [String] -> FilePath
                             -> IO (Maybe ([String],[String]))
rpmspecProvidesBuildRequires lenient rpmopts spec = do
  dynbr <- generateBuildRequires spec
  if dynbr
    then do
    brs <- rpmspecDynBuildRequires spec
    provs <- do
      dynprovs <- dynProvides
      prs <- rpmspecProvides lenient rpmopts spec
      return $ dynprovs ++ prs
    return $ Just (provs,brs)
    else do
    mcontent <- rpmspecParse
    return $ case mcontent of
               Nothing -> Nothing
               Just content ->
                 let pkg = takeBaseName spec
                 in Just $ extractMetadata pkg ([],[]) $ lines content
  where
    extractMetadata :: FilePath -> ([String],[String]) -> [String] -> ([String],[String])
    extractMetadata _ acc [] = acc
    extractMetadata pkg acc@(provs,brs) (l:ls) =
      let ws = words l in
        if length ws < 2 then extractMetadata pkg acc ls
        else case CI.mk (head ws) of
          "BuildRequires:" ->
            let br = (head . tail) ws
            in extractMetadata pkg (provs, br:brs) ls
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

rpmspecDynBuildRequires :: FilePath -> IO [String]
rpmspecDynBuildRequires spec = do
  withTempDir $ \tmpdir -> do
    (out,err) <- cmdStdErr "rpmbuild" ["-br", "--nodeps", "--define", "_srcrpmdir" +-+ tmpdir, spec]
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
