{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# OPTIONS -Wall              #-}


module Main where


import           Control.Lens
import           Control.Monad.Catch
import qualified Data.ByteString.Char8                          as C8
import           Data.Discrimination.Sorting                           (sortWith)
import           Data.Foldable                                         (asum)
import           Data.List                                             ((\\))
import           Data.Maybe
import           Distribution.ModuleName                        hiding (main)
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parsec
import           Distribution.Types.GenericPackageDescription
import           Distribution.Types.ForeignLib
import qualified Distribution.Types.Lens                        as L
import           Distribution.Types.PackageName
import           Distribution.Types.UnqualComponentName
import           System.FilePath


data ExeComp = ExeComp
  { exeCompName    :: String
  , exeCompMainIs  :: String
  , exeCompModules :: [ModuleName]
  , exeCompSrcs    :: [FilePath]
  } deriving (Show)


data FibComp = FibComp
  { fibCompName    :: String
  , fibCompModules :: [ModuleName]
  , fibCompSrcs    :: [FilePath]
  } deriving (Show)


data TstComp = TstComp
  { tstCompName    :: String
  , tstCompMainIs  :: TestSuiteInterface
  , tstCompModules :: [ModuleName]
  , tstCompSrcs    :: [FilePath]
  } deriving (Show)


main :: IO ()
main = do
  -- print =<< getCabalTarget "applied-fp-course.txt" "src"
  -- print =<< getCabalTarget "applied-fp-course.txt" "tests"
  let rootPath = "/Users/taemu/hs_work/nix/edm-haskell/tests/samples/"
  print =<< ("exe:level01-exe" ==) <$>
            getCabalTarget (rootPath <> "applied-fp-course.txt") (rootPath <> "exe/Level01.hs")
  print =<< ("exe:level02-exe" ==) <$>
            getCabalTarget (rootPath <> "applied-fp-course.txt") (rootPath <> "exe/Level02.hs")
  print =<< ("exe:level03-exe" ==) <$>
            getCabalTarget (rootPath <> "applied-fp-course.txt") (rootPath <> "exe/Level03.hs")

  print =<< ("test:app-fp-tests" ==) <$>
            getCabalTarget (rootPath <> "applied-fp-course.txt") (rootPath <> "tests/Foo/Bar/Level03Tests.hs")
  print =<< ("test:doctests" ==) <$>
            getCabalTarget (rootPath <> "applied-fp-course.txt") (rootPath <> "tests/Level08.hs")
  print =<< ("test:app-fp-tests" ==) <$>
            getCabalTarget (rootPath <> "applied-fp-course.txt") (rootPath <> "tests/Test.hs")
  print =<< ("test:doctests" ==) <$>
            getCabalTarget (rootPath <> "applied-fp-course.txt") (rootPath <> "tests/doctest.hs")
  print =<< (\x -> ("test:app-fp-tests" == x) || ("test:doctests" == x)) <$>
            getCabalTarget (rootPath <> "applied-fp-course.txt") (rootPath <> "tests/fooo/Test04.hs")

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

  print =<< ("" ==) <$>
    getCabalTarget "" ""
  print =<< ("" ==) <$>
            getCabalTarget (rootPath <> "sample_cabal.txt") ""
  print =<< ("" ==) <$>
            getCabalTarget "" (rootPath <> "app/pleb2/Bar.hs")
  print =<< ("" ==) <$>
            getCabalTarget "" (rootPath <> "/")
-- TODO: Deal with relative path inputs. Hm Maybe it is bad idea getting relative path input.
--       because we can't know the current absolute path in emacs monad.
--       If we get relative path, then just return nil or throw exception or something.

getCabalTarget :: FilePath -> FilePath -> IO String
getCabalTarget cabalFilePath hsFilePath =  do

  let prjRoot = dropFileName cabalFilePath
      (pwd, hsFile) = splitFileName hsFilePath
      relPath       = joinPath $ splitPath pwd \\ splitPath prjRoot
      hsFileRelPath = relPath </> hsFile

  genPkgsDesc <- fromMaybe emptyGenericPackageDescription . parseGenericPackageDescriptionMaybe
                 <$> catchIOError (C8.readFile cabalFilePath) (return . const mempty)

  let gpkg = genPkgsDesc ^. L.packageDescription . to package . gpkgLens
      libs = genPkgsDesc ^? L.condLibrary . _Just . to condTreeData . libsLens
      exes = genPkgsDesc ^. L.condExecutables ^.. traverse . _2 . to condTreeData . exesLens
      fibs = genPkgsDesc ^. L.condForeignLibs ^.. traverse . _2 . to condTreeData . fibsLens
      tsts = genPkgsDesc ^. L.condTestSuites ^.. traverse . tstsLens

  -- NOTE: How we determine cabal target when loading Test and Benchmakr module?
  --       It turns out that "cabal repl test:..." does exacltly supposed to do.
  -- TODO: We need to examine benchmark and deal with figure out cabal command
 --        options which can run foreign library.

  -- NOTE: If we fail to find proper component name on current path, just return nil.
  let defaultValue = ""
      match = [ if relPath `isAnySubdirOf` libs then Just ("lib:" <> gpkg) else Nothing
              , mkExeTarget "exe:"  hsFileRelPath exes
              , mkFibTarget "flib:" relPath       fibs
              , mkTstTarget "test:" hsFileRelPath tsts ]

  -- NOTE: Maybe is instance of Alternative. So asum returns the first Just value.
  return $ fromMaybe defaultValue (asum match)
  where
    gpkgLens = L.pkgName . to unPackageName
    libsLens = L.hsSourceDirs . to (fmap normalise)
    exesLens = runGetter (ExeComp <$> Getter (L.exeName        . to unUnqualComponentName)
                                  <*> Getter (L.modulePath     . to normalise            )
                                  <*> Getter (to exeModules                              )
                                  <*> Getter (L.hsSourceDirs   . to (fmap normalise))    )
    fibsLens = runGetter (FibComp <$> Getter (L.foreignLibName . to unUnqualComponentName)
                                  <*> Getter (to foreignLibModules                       )
                                  <*> Getter (L.hsSourceDirs   . to (fmap normalise))    )
    tstsLens = runGetter (TstComp <$> Getter (_1 . to unUnqualComponentName              )
                                  <*> Getter (_2 . to condTreeData . L.testInterface     )
                                  <*> Getter (_2 . to condTreeData . to testModules      )
                                  <*> Getter (_2 . to condTreeData . L.hsSourceDirs  . to (fmap normalise)))


isAnySubdirOf :: FilePath -> Maybe [FilePath] -> Bool
isAnySubdirOf p = maybe False (p `hasSuperDir`)


-- NOTE: Does exe component has also combinatoric problem?
mkExeTarget :: String -> FilePath -> [ExeComp] -> Maybe String
mkExeTarget prefix relPathFile comps = (<>) prefix . exeCompName <$> exactOrClosest
  where
    -- NOTE: -- If main-is field is set, then exact match is must be prioritized.
    -- TODO: What if mains-is set and there's no match?
    --       Should we return empty string or just make best guess?
    --       Currently we just return best match.
    hsFile           = takeFileName relPathFile
    relPath          = dropFileName relPathFile
    -- exactOrClosest   = asumOf each (mainIsSameParent, closestParent)
    exactOrClosest   = asumOf each (mainIsSameParent, closestParent)
    closestParent    = firstOf traverse parentCandidates
    mainIsSameParent = findOf traverse  ((== hsFile) . exeCompMainIs) parentCandidates
    parentCandidates = toListOf (traverse . filtered ((relPath `hasSuperDir`) . exeCompSrcs)) (sortLongest comps)
    -- NOTE: Longest path is the closest path to the target path.
    -- ex) The closest path to "app/mkCabalTarget/bar/wat" is "app/mkCabalTarget/bar"
    --     among "app", "app/mkCabalTarget" and "app/mkCabalTarget/bar".
    -- TODO: Make sortLongest polymorphic over field. Maybe use makeFields from lens library.
    sortLongest :: [ExeComp] -> [ExeComp]
    sortLongest = sortWith ((\x -> x *(-1)) . maximum . fmap length . exeCompSrcs)


mkFibTarget :: String -> FilePath -> [FibComp] -> Maybe String
mkFibTarget prefix path comps = (<>) prefix . fibCompName <$> closestParent
  where
    closestParent    = firstOf traverse parentCandidates
    parentCandidates = toListOf (traverse . filtered ((path `hasSuperDir`) . fibCompSrcs)) (sortLongest comps)
    sortLongest :: [FibComp] -> [FibComp]
    sortLongest = sortWith ((\x -> x *(-1)) . maximum . fmap length . fibCompSrcs)


mkTstTarget :: String -> FilePath -> [TstComp] -> Maybe String
mkTstTarget prefix relPathFile comps = (<>) prefix . tstCompName <$> exactOrClosest
  where
    exactOrClosest   = asumOf each (mainIsSameParent, closestParent)
    mainIsSameParent = findOf traverse (matchTestComp relPathFile) parentCandidates
    closestParent    = firstOf traverse parentCandidates
    parentCandidates = toListOf (traverse . filtered ((relPathFile `hasSuperDir`) . tstCompSrcs)) (sortLongest comps)
    -- TODO: Find out which combinations are valid is important. Otherwise we will be just rolling the wheel.
    sortLongest :: [TstComp] -> [TstComp]
    sortLongest = sortWith ((\x -> x *(-1)) . maximum . fmap length . tstCompSrcs)


matchTestComp :: FilePath -> TstComp -> Bool
matchTestComp relPathFile tstComp = hsFileName == tstCompHsFileName
                                 || elemOf traverse relPathWithoutExt hsSourceDirsModulesCombinations
  where
    -- NOTE: How do we deal with combinatoric explosion with hs-source-dirs and component path?
    --       What is the legit cases of this situation?
    --       Same module Path with difference hs-source-dirs is correct cabal configuration?
    --       For now, we just assume that all of the combinations are valid.
    -- TODO : Figure out what is like setting TestSuiteLibV09.
    hsSourceDirsModulesCombinations = do srcRel  <- tstCompSrcs tstComp
                                         compRel <- toFilePath <$> tstCompModules tstComp
                                         return $ srcRel </> compRel
    hsFileName          = takeFileName relPathFile
    relPathWithoutExt   = dropExtension relPathFile
    tstCompHsFileName   = case tstCompMainIs tstComp of
        (TestSuiteExeV10 _ hsName) -> hsName
        _ -> ""
        -- (TestSuiteLibV09 _ moduleName) -> dropExtension relPathFile == toFilePath moduleName
        -- (TestSuiteUnsupported _)       -> False


hasSuperDir :: FilePath -> [FilePath] -> Bool
hasSuperDir p = anyOf traverse (p `isSubDirOf`)


isSubDirOf :: FilePath -> FilePath -> Bool
isSubDirOf child parent = length parent' == length (takeWhile id ee)
    where
    child' = splitDirectories child
    parent' = splitDirectories parent
    ee = zipWith (==) child' parent'
