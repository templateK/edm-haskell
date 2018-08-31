{-# LANGUAGE OverloadedStrings #-}


module Main where


import Control.Lens
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Verbosity
import Distribution.Types.UnqualComponentName
import qualified Distribution.Types.Lens    as L
import Distribution.Types.PackageName
import Data.Maybe
import System.FilePath
import Data.List ((\\), sortBy)
import Data.Ord (Down(..), comparing)


main :: IO ()
main = do
  print =<< ("exe:top"   ==) <$> getCabalTarget "sample_cabal.txt" "app"
  print =<< ("exe:pleb1" ==) <$> getCabalTarget "sample_cabal.txt" "app/pleb1"
  print =<< ("exe:pleb2" ==) <$> getCabalTarget "sample_cabal.txt" "app/pleb2"

-- NOTE: Don't deal with specific file. Only depend on path.
--       You may be have to deal with file searching on
--       current directory if you want to check the filename.
getCabalTarget :: FilePath -> FilePath -> IO String
getCabalTarget cabalFilePath pwd = do

  let prjRoot = dropFileName cabalFilePath
      relPath = joinPath $ splitPath pwd \\ splitPath prjRoot

  genPkgsDesc <- readGenericPackageDescription normal cabalFilePath

  let gpkg = genPkgsDesc ^. L.packageDescription
                         . to package
                         . gpkgLens

      libs = genPkgsDesc ^. L.condLibrary
                         ^? _Just
                         . to condTreeData
                         . libsLens

      exes = genPkgsDesc ^. L.condExecutables
                         ^.. folded
                         . _2
                         . to condTreeData
                         . exesLens
  -- print $ libs
  -- print $ exes
  -- NOTE: For now, if we fail to find proper component name on current path,
  --       We use empty string as the default the value of the cabal target.
  --       Or? Just return emtpy string?? or... Maybe value
  return $ if relPath `isSubOf` libs
             then "lib:" <> gpkg
             else fromMaybe "" $ ((<>) "exe:" . fst) <$> (relPath `compOf` exes)
  where
    hasChildIn p   = anyOf folded (p `isParentDirOf`)
    isSubOf p dirs = fromMaybe False ((p `hasChildIn`) <$> dirs)
    compOf p dirs  = firstOf (traverse . filtered ((p `hasChildIn`) . snd)) $ sortLongest dirs
    gpkgLens = L.pkgName . to unPackageName
    libsLens = L.hsSourceDirs . to (fmap normalise)
    exesLens = runGetter $ (,)
                <$> Getter (L.exeName . to unUnqualComponentName)
                <*> Getter (L.hsSourceDirs . to (fmap normalise))


isParentDirOf :: FilePath -> FilePath -> Bool
isParentDirOf parent child = length child' == (length $ takeWhile id ee)
  where
    parent' = splitDirectories parent
    child' = splitDirectories child
    ee = zipWith (==) parent' child'


-- Longest path is the closest path to the target path.
-- ex) The closest path to "app/foo/bar/wat" is "app/foo/bar"
--     among "app", "app/foo" and "app/foo/bar".
sortLongest :: [(a, [FilePath])] -> [(a, [FilePath])]
sortLongest = sortBy (comparing $ Down . maximum . fmap length . snd)
