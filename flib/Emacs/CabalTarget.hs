{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE ScopedTypeVariables      #-}


module Emacs.CabalTarget (initialise) where

import Control.Monad.IO.Class
import Data.Emacs.Module.Args
import Data.Emacs.Module.SymbolName.TH
import Emacs.Module
import Emacs.Module.Assert
import Emacs.Module.Errors
import qualified Data.ByteString.Char8 as C8

import Control.Lens
import Control.Monad.Catch
import Distribution.ModuleName hiding (main)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Simple.Utils
import Distribution.Types.ForeignLib
import Distribution.Types.UnqualComponentName
import Distribution.Types.PackageName
import Distribution.Types.GenericPackageDescription
import qualified Distribution.Types.Lens    as L

import System.FilePath
import Data.List ((\\), sortOn)
import Data.Ord (Down(..))
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)


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


initialise
  :: (WithCallStack, Throws EmacsThrow, Throws EmacsError, Throws EmacsInternalError)
  => EmacsM s ()
initialise =
  bindFunction [esym|edm-haskell-cabal-target|] =<<
    makeFunction getCabalTarget getCabalTargetDoc


getCabalTargetDoc :: C8.ByteString
getCabalTargetDoc =
  "return cabal command target from current directory."


getCabalTarget
  :: forall m s. (WithCallStack, MonadCatch (m s), MonadThrow (m s), MonadEmacs m, Monad (m s), MonadIO (m s))
  => EmacsFunction ('S ('S 'Z)) 'Z 'False s m
getCabalTarget (R cabalFilePathRef (R currentDirRef Stop)) = do
  cabalPath  <- fromUTF8BS <$> extractString cabalFilePathRef
  hsFilePath <- fromUTF8BS <$> extractString currentDirRef
  let prjRoot = dropFileName cabalPath
      (pwd, hsFile) = splitFileName hsFilePath
      relPath       = joinPath $ splitPath pwd \\ splitPath prjRoot
      hsFileRelPath = relPath </> hsFile

  genPkgsDesc <- liftIO $ fromMaybe emptyGenericPackageDescription . parseGenericPackageDescriptionMaybe
                        <$> catchIOError (C8.readFile cabalPath) (return . const mempty)

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
  let match = [ if relPath `isAnySubdirOf` libs then Just ("lib:" <> gpkg) else Nothing
              , mkExeTarget "exe:"  hsFileRelPath exes
              , mkFibTarget "flib:" relPath       fibs
              , mkTstTarget "test:" hsFileRelPath tsts ]
  -- NOTE: Maybe is instance of Alternative. So asum returns the first Just value.
  produceRef =<< maybe (intern [esym|nil|]) (makeString . toUTF8BS) (asum match)
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
    exactOrClosest   = asumOf each (mainIsSameParent, closestParent)
    closestParent    = firstOf traverse parentCandidates
    mainIsSameParent = findOf traverse  ((== hsFile) . exeCompMainIs) parentCandidates
    parentCandidates = getCandiates comps relPath exeCompSrcs


-- NOTE: Longest path is the closest path to the target path.
-- ex) The closest path to "app/mkCabalTarget/bar/wat" is "app/mkCabalTarget/bar"
--     among "app", "app/mkCabalTarget" and "app/mkCabalTarget/bar".
getCandiates :: [a] -> FilePath -> (a -> [FilePath]) -> [a]
getCandiates cs path getter = sortByLengthDescend getter cs ^.. traverse . filtered ((path `hasSuperDir`) . getter)
  where
    sortByLengthDescend :: (Functor t, Foldable t, Foldable t1) => (a1 -> t (t1 a)) -> [a1] -> [a1]
    sortByLengthDescend f = sortOn (Down . maximum . fmap length . f)


mkFibTarget :: String -> FilePath -> [FibComp] -> Maybe String
mkFibTarget prefix relPath comps = (<>) prefix . fibCompName <$> closestParent
  where
    closestParent    = firstOf traverse parentCandidates
    parentCandidates = getCandiates comps relPath fibCompSrcs


mkTstTarget :: String -> FilePath -> [TstComp] -> Maybe String
mkTstTarget prefix relPath comps = (<>) prefix . tstCompName <$> exactOrClosest
  where
    exactOrClosest   = asumOf each (mainIsSameParent, closestParent)
    mainIsSameParent = findOf traverse (matchTestComp relPath) parentCandidates
    closestParent    = firstOf traverse parentCandidates
    parentCandidates = getCandiates comps relPath tstCompSrcs


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
isSubDirOf child parent = length parent' == length (takeWhile id commonPath)
    where
    child'     = splitDirectories child
    parent'    = splitDirectories parent
    commonPath = zipWith (==) child' parent'
