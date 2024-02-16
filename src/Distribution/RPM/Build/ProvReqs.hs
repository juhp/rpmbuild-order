{-# LANGUAGE CPP, OverloadedStrings #-}

module Distribution.RPM.Build.ProvReqs
  (rpmspecDynBuildRequires,
   rpmspecProvidesBuildRequires)
where

import Control.Monad (unless)
import qualified Data.CaseInsensitive as CI
import Data.List.Extra
import Data.Maybe (mapMaybe)
import SimpleCmd (cmdFull, cmdLines, cmdMaybe, cmdStdErr, egrep_, error',
                  grep, warning, (+-+))
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.FilePath
import System.IO.Extra (withTempDir)

generateBuildRequires :: FilePath -> IO Bool
generateBuildRequires =
  egrep_ "^\\(%generate_buildrequires\\|%gometa\\)"

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
    return $ Just (provs, brs)
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
      let ws = words l in
        case length ws of
          0 -> extractMetadata pkg acc ls
          1 ->
            if ".pc" `isSuffixOf` head ws
            then do
              pcs <- map (\p -> "pkgconfig(" ++ takeBaseName p ++ ")") <$>
                     egrep "^%{\\(_libdir\\|_datadir\\)}/pkgconfig/.*\\.pc" spec
              extractMetadata pkg (provs ++ pcs, brs) ls
            else extractMetadata pkg acc ls
          _ ->
            case CI.mk (head ws) of
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

    simplifyDep br =
      case (head . words) br of
        '(':dep -> simplifyDep dep
        dep -> case splitOn "(" (dropSuffix ")" dep) of
          ("rpmlib":_) -> Nothing
          ("crate":[crate]) -> Just $ "rust-" ++ replace "/" "+" crate ++ "-devel"
          ("rubygem":[gem]) -> Just $ "rubygem-" ++ gem
          _ -> Just dep

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

#if !MIN_VERSION_simple_cmd(0,2,8)
egrep :: String -> FilePath -> IO [String]
egrep regexp file = do
  mres <- cmdMaybe "grep" ["-e", regexp, file]
  return $ maybe [] lines mres
#endif
