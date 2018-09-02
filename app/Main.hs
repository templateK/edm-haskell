{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}


module Main where


import Control.Lens
import Distribution.PackageDescription hiding (exeName)
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


data ExeComp = ExeComp
  { exeName :: String
  , mainIs  :: String
  , srcDirs :: [FilePath]
  } deriving (Show)



main :: IO ()
main = do
  -- print =<< getCabalTarget "applied-fp-course.txt" "src"
  -- print =<< getCabalTarget "applied-fp-course.txt" "tests"
  print =<< getCabalTarget "applied-fp-course.txt" "tests"
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
      exes = genPkgsDesc ^. L.condExecutables  ^.. folded . _2 . to condTreeData . exesLens
      -- sibs = genPkgsDesc ^. L.condSubLibraries ^.. sibsLens
      -- fibs = genPkgsDesc ^. L.condForeignLibs  ^.. fibsLens
  -- NOTE: For now, if we fail to find proper component name on current path,
  --       We use empty string as the default the value of the cabal target.
  --       Or? Just return emtpy string?? or... Maybe value
  let defaultValue = ""
      match = [ if relPath `isAnySubdirOf` libs then Just ("lib:" <> gpkg) else Nothing
              , mkExeTarget relPath "exe:"   exes ] 
              -- , mkCabalTarget relPath "lib:"   sibs -- sub library qualfication is `lib`
              -- , mkCabalTarget relPath "flib:"  fibs ]
  -- NOTE: Maybe is instance of Alternative. So asum returns the first Just value.
  return $ fromMaybe defaultValue (asum match)
  where
    gpkgLens = L.pkgName . to unPackageName
    libsLens = L.hsSourceDirs . to (fmap normalise)
    exesLens = runGetter (ExeComp <$> Getter (L.exeName      . to unUnqualComponentName)
                                  <*> Getter (L.modulePath   . to normalise            )
                                  <*> Getter (L.hsSourceDirs . to (fmap normalise))    )
    -- compLens gttr =
    --   folded . _2 . to condTreeData
    --          . runGetter ((,) <$> gttr <*> Getter (L.hsSourceDirs . to (fmap normalise)))

isAnySubdirOf :: FilePath -> Maybe [FilePath] -> Bool
isAnySubdirOf p dirs = fromMaybe False ((p `hasChildIn`) <$> dirs)


mkExeTarget :: FilePath -> String -> [ExeComp] -> Maybe String
mkExeTarget path prefix comps = ((<>) prefix . exeName) <$> (path `sameOrClosest` comps)
  where
    sameOrClosest p cs = asum (path `samePathOf` comps , path `closestOf` cs)
    closestOf  p cs    = firstOf traverse (p `subDirOf` cs) 
    samePathOf p cs    = findOf traverse  (\x ->  p ==  mainIs x) (p `subDirOf` cs) 
    subDirOf p cs      = toListOf (traverse . filtered ((p `hasChildIn`) . srcDirs)) (sortLongest cs) 
    -- NOTE: Longest path is the closest path to the target path.
    -- ex) The closest path to "app/mkCabalTarget/bar/wat" is "app/mkCabalTarget/bar"
    --     among "app", "app/mkCabalTarget" and "app/mkCabalTarget/bar".
    sortLongest :: [ExeComp] -> [ExeComp]
    sortLongest = sortBy (comparing $ Down . maximum . fmap length . srcDirs)


hasChildIn :: FilePath -> [FilePath] -> Bool
hasChildIn p = anyOf folded (p `isParentDirOf`)
  where
    isParentDirOf :: FilePath -> FilePath -> Bool
    isParentDirOf parent child = length child' == (length $ takeWhile id ee)
      where
        parent' = splitDirectories parent
        child' = splitDirectories child
        ee = zipWith (==) parent' child'
