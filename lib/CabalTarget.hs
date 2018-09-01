{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE ScopedTypeVariables      #-}


module CabalTarget (initialise) where

-- import Control.Monad
import Control.Monad.IO.Class
-- import qualified Control.Exception.Safe.Checked as Checked
import Data.Emacs.Module.Args
import Data.Emacs.Module.SymbolName.TH
import Emacs.Module
import Emacs.Module.Assert
import Emacs.Module.Errors
import qualified Data.ByteString.Char8 as C8

import Control.Lens
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.Types.UnqualComponentName
import Distribution.Types.PackageName
import Distribution.Types.GenericPackageDescription
import qualified Distribution.Types.Lens    as L

import System.Directory
import System.FilePath

import Data.List ((\\), sortBy)
import Data.Maybe
import Data.Ord (Down(..), comparing)
import Data.Foldable (asum)
-- import Data.Text.Prettyprint.Doc (pretty, (<+>))

-- import Control.Concurrent.Async.Lifted.Safe
-- import Control.Concurrent.STM
-- import Control.Concurrent.STM.TMQueue
-- import qualified Control.Exception.Safe.Checked as Checked
-- import Control.Monad.Base
-- import Control.Monad.Trans.Control

-- import Data.Text.Prettyprint.Doc
-- import Data.Traversable
-- import GHC.Conc (getNumCapabilities)



initialise
  :: (WithCallStack, Throws EmacsThrow, Throws EmacsError, Throws EmacsInternalError)
  => EmacsM s ()
initialise =
  bindFunction [esym|emacs-dyn-cabal-target|] =<<
    makeFunction getCabalTarget getCabalTargetDoc


getCabalTargetDoc :: C8.ByteString
getCabalTargetDoc =
  "return cabal command target from current directory."


-- NOTE: Don't deal with specific file. Only depend on path.
--       You may be have to deal with file searching on
--       current directory if you want to check the filename.
getCabalTarget
  :: forall m s. (WithCallStack, MonadThrow (m s), MonadEmacs m, Monad (m s), MonadIO (m s))
  => EmacsFunction ('S ('S 'Z)) 'Z 'False s m
getCabalTarget (R cabalFilePathRef (R currentDirRef Stop)) = do

  pwd       <- fromUTF8BS <$> extractString currentDirRef
  cabalPath <- fromUTF8BS <$> extractString cabalFilePathRef

  let prjRoot = dropFileName cabalPath
      relPath = joinPath $ splitPath pwd \\ splitPath prjRoot

  -- TODO: From testing the error, it shows eslip debug buffer.
  --       Once exception occurs, the  dynamic module won't recover from it.
  --       How to recover from previous error?
  --       For now, just use empty package when there's no cabal package file.
  -- cabalPath <- do
  --   exists <- liftIO $ doesPathExist cabalFilePath
  --   unless exists $
  --     Checked.throw $ mkUserError "emacsGrepRec" $
  --       "Cabal file does not exist: " <+> pretty cabalFilePath
  --   return cabalFilePath

  genPkgsDesc   <- liftIO $ do
    exists <- doesPathExist cabalPath
    if exists
      then readGenericPackageDescription normal cabalPath
      else return emptyGenericPackageDescription

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
  produceRef =<< (makeString . toUTF8BS) (fromMaybe defaultValue (asum match))
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


mkCabalTarget :: Semigroup a => FilePath -> a -> [(a, [FilePath])] -> Maybe a
mkCabalTarget path prefix dirs = ((<>) prefix . fst) <$> (path `compOf` dirs)


-- NOTE: Longest path is the closest path to the target path.
-- ex) The closest path to "app/mkCabalTarget/bar/wat" is "app/mkCabalTarget/bar"
--     among "app", "app/mkCabalTarget" and "app/mkCabalTarget/bar".
compOf :: FilePath -> [(a, [FilePath])] -> Maybe (a, [FilePath])
compOf p dirs = firstOf (traverse . filtered ((p `hasChildIn`) . snd)) $ sortLongest dirs
  where
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
