module Main where

import Control.Lens
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Verbosity
import Distribution.Types.UnqualComponentName
import qualified Distribution.Types.Lens    as L
import Distribution.Types.PackageName
import Data.Maybe


main :: IO ()
main = do
  let filename = "sample_cabal.txt"
  foo <- getCabalTarget filename "src"
  print foo


-- NOTE: Don't deal with specific file. Only depend on path.
--       You may be have to deal with file searching on
--       current directory if you want to check the filename.
getCabalTarget :: FilePath -> FilePath -> IO (Maybe String)
getCabalTarget cabalFilePath path = do

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
  -- NOTE: For now, if we fail to find proper component name on current path,
  --       We use exe:<global package name> as the default the value of the cabal target.
  --       Or? Just return emtpy string?? or... Maybe value
  return $ if path `isSubOf` libs
             then Just $ "lib:" <> gpkg
             else (<>) "exe:" <$> (path `compOf` exes)
  where
    -- TODO: proper subdirectory testing
    isSubOf p dirs = fromMaybe False (elemOf folded p <$> dirs)
    compOf p dirs  = fst <$> findOf folded (elem p . snd) dirs
    gpkgLens = L.pkgName . to unPackageName
    libsLens = L.hsSourceDirs
    exesLens = runGetter $ (,)
                <$> Getter (L.exeName . to unUnqualComponentName)
                <*> Getter L.hsSourceDirs
