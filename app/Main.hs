{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}


module Main where


import Control.Lens
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.PackageDescription.PrettyPrint
import Distribution.Verbosity
import Distribution.Types.UnqualComponentName
import qualified Distribution.Types.Lens    as L
import Distribution.Types.PackageName
import Data.Maybe
import System.FilePath
import Data.List ((\\), sortBy)
import Data.Ord (Down(..), comparing)
import Data.Foldable (asum)


main :: IO ()
main = do
  print =<< getCabalTarget "applied-fp-course.txt" "src"
  print =<< getCabalTarget "applied-fp-course.txt" "tests"
  print =<< getCabalTarget "applied-fp-course.txt" "exe"
  -- print =<< ("flib:fcomp"==) <$> getCabalTarget "foreign-library-cabal.txt" "lib"
  -- print =<< ("flib:fcomp"==) <$> getCabalTarget "foreign-library-cabal.txt" "lib/foo/bar"
  -- print =<< ("exe:top"   ==) <$> getCabalTarget "sample_cabal.txt" "app"
  -- print =<< ("exe:pleb1" ==) <$> getCabalTarget "sample_cabal.txt" "app/pleb1"
  -- print =<< ("exe:pleb2" ==) <$> getCabalTarget "sample_cabal.txt" "app/pleb2"


-- NOTE: Don't deal with specific file. Only depend on path.
--       You may be have to deal with file searching on
--       current directory if you want to check the filename.
getCabalTarget :: FilePath -> FilePath -> IO String
getCabalTarget cabalFilePath pwd = do

  let prjRoot = dropFileName cabalFilePath
      relPath = joinPath $ splitPath pwd \\ splitPath prjRoot

  genPkgsDesc <- readGenericPackageDescription normal cabalFilePath
  -- print $ genPkgsDesc

  let gpkg = genPkgsDesc ^. L.packageDescription . to package . gpkgLens
      libs = genPkgsDesc ^. L.condLibrary ^? _Just . to condTreeData . libsLens
      sibs = genPkgsDesc ^. L.condSubLibraries ^.. sibsLens
      fibs = genPkgsDesc ^. L.condForeignLibs  ^.. fibsLens
      exes = genPkgsDesc ^. L.condExecutables  ^.. exesLens
      tsts = genPkgsDesc ^. L.condTestSuites   ^.. tstsLens
      bchs = genPkgsDesc ^. L.condBenchmarks   ^.. bchsLens
  -- NOTE: For now, if we fail to find proper component name on current path,
  --       We use empty string as the default the value of the cabal target.
  --       Or? Just return emtpy string?? or... Maybe value
  let defaultValue = ""
      match = [ if relPath `isAnySubdirOf` libs then Just ("lib:" <> gpkg) else Nothing
              , mkCabalTarget relPath "lib:"   sibs -- sub library qualfication is `lib`
              , mkCabalTarget relPath "exe:"   exes
              , mkCabalTarget relPath "flib:"  fibs
              , mkCabalTarget relPath "test:"  tsts
              , mkCabalTarget relPath "bench:" bchs ]
  -- NOTE: Maybe is instance of Alternative. So asum returns the first Just value.
  return $ fromMaybe defaultValue (asum match)
  where
    gpkgLens = L.pkgName . to unPackageName
    libsLens = L.hsSourceDirs . to (fmap normalise)
    exesLens = compLens $ Getter (L.exeName          . to unUnqualComponentName)
    fibsLens = compLens $ Getter (L.foreignLibName   . to unUnqualComponentName)
    sibsLens = compLens $ Getter (L.libName . non "" . to unUnqualComponentName)
    tstsLens = compLens $ Getter (L.testName         . to unUnqualComponentName)
    bchsLens = compLens $ Getter (L.benchmarkName    . to unUnqualComponentName)
    compLens gttr =
      folded . _2 . to condTreeData
             . runGetter ((,) <$> gttr <*> Getter (L.hsSourceDirs . to (fmap normalise)))


isAnySubdirOf :: FilePath -> Maybe [FilePath] -> Bool
isAnySubdirOf p dirs = fromMaybe False ((p `hasChildIn`) <$> dirs)


mkCabalTarget :: Semigroup b => FilePath -> b -> [(b, [FilePath])] -> Maybe b
mkCabalTarget path prefix dirs = ((<>) prefix . fst) <$> (path `compOf` dirs)
  where
    compOf :: FilePath -> [(a, [FilePath])] -> Maybe (a, [FilePath])
    compOf p dirs  = firstOf (traverse . filtered ((p `hasChildIn`) . snd))
                       $ sortLongest dirs
    -- NOTE: Longest path is the closest path to the target path.
    -- ex) The closest path to "app/mkCabalTarget/bar/wat" is "app/mkCabalTarget/bar"
    --     among "app", "app/mkCabalTarget" and "app/mkCabalTarget/bar".
    sortLongest :: [(a, [FilePath])] -> [(a, [FilePath])]
    sortLongest = sortBy (comparing $ Down . maximum . fmap length . snd)


hasChildIn :: FilePath -> [FilePath] -> Bool
hasChildIn p = anyOf folded (p `isParentDirOf`)
  where
    isParentDirOf :: FilePath -> FilePath -> Bool
    isParentDirOf parent child = length child' == (length $ takeWhile id ee)
      where
        parent' = splitDirectories parent
        child' = splitDirectories child
        ee = zipWith (==) parent' child'
