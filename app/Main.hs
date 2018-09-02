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
  { exeName   :: String
  , exeMainIs :: String
  , exeSrcs   :: [FilePath]
  } deriving (Show)


data FibComp = FibComp
  { fibName :: String
  , fibSrcs :: [FilePath]
  } deriving (Show)


main :: IO ()
main = do
  -- print =<< getCabalTarget "applied-fp-course.txt" "src"
  -- print =<< getCabalTarget "applied-fp-course.txt" "tests"
  let rootPath = "/Users/taemu/hs_work/nix/emacs-dyn-cabal/"
  print =<< ("exe:level01-exe" ==) <$>
            getCabalTarget (rootPath <> "applied-fp-course.txt") (rootPath <> "exe/Level01.hs")
  print =<< ("exe:level02-exe" ==) <$>
            getCabalTarget (rootPath <> "applied-fp-course.txt") (rootPath <> "exe/Level02.hs")
  print =<< ("exe:level03-exe" ==) <$>
            getCabalTarget (rootPath <> "applied-fp-course.txt") (rootPath <> "exe/Level03.hs")
  print =<< ("flib:fcomp"==) <$>
            getCabalTarget (rootPath <> "foreign-library-cabal.txt") (rootPath <> "lib/Zeez.hs")
  print =<< ("flib:fcomp"==) <$>
            getCabalTarget (rootPath <> "foreign-library-cabal.txt") (rootPath <> "lib/foo/bar/Wat.hs")
  print =<< ("exe:top"   ==) <$>
            getCabalTarget (rootPath <> "sample_cabal.txt") (rootPath <> "app/What.hs")
  print =<< ("exe:pleb1" ==) <$>
            getCabalTarget (rootPath <> "sample_cabal.txt") (rootPath <> "app/pleb1/Foo.hs")
  print =<< ("exe:pleb2" ==) <$>
            getCabalTarget (rootPath <> "sample_cabal.txt") (rootPath <> "app/pleb2/Bar.hs")


-- NOTE: Don't deal with specific file. Only depend on path.
--       You may be have to deal with file searching on
--       current directory if you want to check the filename.
--       cabalFilePath and hsFilePath must be in the form of full path. 
-- TODO: Deal with relative path inputs.
getCabalTarget :: FilePath -> FilePath -> IO String
getCabalTarget cabalFilePath hsFilePath =  do

  let prjRoot = dropFileName cabalFilePath
      (pwd, mainIsFileName)  = splitFileName hsFilePath
      relPath = joinPath $ splitPath pwd \\ splitPath prjRoot

  genPkgsDesc <- readGenericPackageDescription normal cabalFilePath
  -- print $ genPkgsDesc

  let gpkg = genPkgsDesc ^. L.packageDescription . to package . gpkgLens
      libs = genPkgsDesc ^. L.condLibrary ^? _Just . to condTreeData . libsLens
      exes = genPkgsDesc ^. L.condExecutables  ^.. folded . _2 . to condTreeData . exesLens
      fibs = genPkgsDesc ^. L.condForeignLibs  ^.. folded . _2 . to condTreeData . fibsLens
  -- TODO: How we determine cabal target when loading Test and Benchmakr module.
  --       cabal repl test:... doesn't make sense because it just run tests.

  -- NOTE: For now, if we fail to find proper component name on current path,
  --       We use empty string as the default the value of the cabal target.
  --       Or? Just return emtpy string?? or... Maybe value
  let defaultValue = ""
      match = [ if relPath `isAnySubdirOf` libs then Just ("lib:" <> gpkg) else Nothing
              , mkExeTarget "exe:"  relPath mainIsFileName exes
              , mkFibTarget "flib:" relPath fibs ] 
  -- NOTE: Maybe is instance of Alternative. So asum returns the first Just value.
  return $ fromMaybe defaultValue (asum match)
  where
    gpkgLens = L.pkgName . to unPackageName
    libsLens = L.hsSourceDirs . to (fmap normalise)
    exesLens = runGetter (ExeComp <$> Getter (L.exeName        . to unUnqualComponentName)
                                  <*> Getter (L.modulePath     . to normalise            )
                                  <*> Getter (L.hsSourceDirs   . to (fmap normalise))    )
    fibsLens = runGetter (FibComp <$> Getter (L.foreignLibName . to unUnqualComponentName)
                                  <*> Getter (L.hsSourceDirs   . to (fmap normalise))    )


isAnySubdirOf :: FilePath -> Maybe [FilePath] -> Bool
isAnySubdirOf p dirs = fromMaybe False ((p `hasChildIn`) <$> dirs)


mkExeTarget :: String -> FilePath -> FilePath -> [ExeComp] -> Maybe String
mkExeTarget prefix path file comps = ((<>) prefix . exeName) <$> sameOrClosest
  where
    -- NOTE: -- If main-is field is set, then exact match is must be prioritized.
    -- TODO: What if mains-is set and there's no match?
    --       Should we return empty string or just make best guess?
    --       Currently we just return best match.
    sameOrClosest    = asumOf each (mainIsSameParent, closestParent)
    closestParent    = firstOf traverse parentCandidates
    mainIsSameParent = findOf traverse  ((== file) . exeMainIs) parentCandidates
    parentCandidates = toListOf (traverse . filtered ((path `hasChildIn`) . exeSrcs)) (sortLongest comps) 
    -- NOTE: Longest path is the closest path to the target path.
    -- ex) The closest path to "app/mkCabalTarget/bar/wat" is "app/mkCabalTarget/bar"
    --     among "app", "app/mkCabalTarget" and "app/mkCabalTarget/bar".
    sortLongest :: [ExeComp] -> [ExeComp]
    sortLongest = sortBy (comparing $ Down . maximum . fmap length . exeSrcs)


mkFibTarget :: String -> FilePath -> [FibComp] -> Maybe String
mkFibTarget prefix path comps = ((<>) prefix . fibName) <$> closestParent
  where
    closestParent    = firstOf traverse parentCandidates
    parentCandidates = toListOf (traverse . filtered ((path `hasChildIn`) . fibSrcs)) (sortLongest comps) 
    sortLongest :: [FibComp] -> [FibComp]
    sortLongest = sortBy (comparing $ Down . maximum . fmap length . fibSrcs)


hasChildIn :: FilePath -> [FilePath] -> Bool
hasChildIn p = anyOf folded (p `isParentDirOf`)
  where
    isParentDirOf :: FilePath -> FilePath -> Bool
    isParentDirOf parent child = length child' == (length $ takeWhile id ee)
      where
        parent' = splitDirectories parent
        child' = splitDirectories child
        ee = zipWith (==) parent' child'
