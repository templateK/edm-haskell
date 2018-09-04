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

data ExeComp = ExeComp
  { exeCompName   :: String
  , exeCompMainIs :: String
  , exeCompSrcs   :: [FilePath]
  } deriving (Show)


data FibComp = FibComp
  { fibCompName :: String
  , fibCompSrcs :: [FilePath]
  } deriving (Show)


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
-- TODO: return nil when there's no appropriate target.
getCabalTarget
  :: forall m s. (WithCallStack, MonadThrow (m s), MonadEmacs m, Monad (m s), MonadIO (m s))
  => EmacsFunction ('S ('S 'Z)) 'Z 'False s m
getCabalTarget (R cabalFilePathRef (R currentDirRef Stop)) = do

  cabalPath  <- fromUTF8BS <$> extractString cabalFilePathRef
  hsFilePath <- fromUTF8BS <$> extractString currentDirRef
  let prjRoot = dropFileName cabalPath
      (pwd, hsFile) = splitFileName hsFilePath
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
      exes = genPkgsDesc ^. L.condExecutables  ^.. folded . _2 . to condTreeData . exesLens
      fibs = genPkgsDesc ^. L.condForeignLibs  ^.. folded . _2 . to condTreeData . fibsLens
  -- TODO: How we determine cabal target when loading Test and Benchmakr module.
  --       cabal repl test:... doesn't make sense because it just run tests.

  -- NOTE: For now, if we fail to find proper component name on current path,
  --       We use empty string as the default the value of the cabal target.
  --       Or? Just return emtpy string?? or... Maybe value
  let defaultValue = ""
      match = [ if relPath `isAnySubdirOf` libs then Just ("lib:" <> gpkg) else Nothing
              , mkExeTarget "exe:"  relPath hsFile exes
              , mkFibTarget "flib:" relPath fibs ] 
  -- NOTE: Maybe is instance of Alternative. So asum returns the first Just value.
  produceRef =<< (makeString . toUTF8BS) (fromMaybe defaultValue (asum match))
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
mkExeTarget prefix path file comps = ((<>) prefix . exeCompName) <$> sameOrClosest
  where
    -- NOTE: -- If main-is field is set, then exact match is must be prioritized.
    -- TODO: What if mains-is set and there's no match?
    --       Should we return empty string or just make best guess?
    --       Currently we just return best match.
    sameOrClosest    = asumOf each (mainIsSameParent, closestParent)
    closestParent    = firstOf traverse parentCandidates
    mainIsSameParent = findOf traverse  ((== file) . exeCompMainIs) parentCandidates
    parentCandidates = toListOf (traverse . filtered ((path `hasChildIn`) . exeCompSrcs)) (sortLongest comps) 
    -- NOTE: Longest path is the closest path to the target path.
    -- ex) The closest path to "app/mkCabalTarget/bar/wat" is "app/mkCabalTarget/bar"
    --     among "app", "app/mkCabalTarget" and "app/mkCabalTarget/bar".
    sortLongest :: [ExeComp] -> [ExeComp]
    sortLongest = sortBy (comparing $ Down . maximum . fmap length . exeCompSrcs)


mkFibTarget :: String -> FilePath -> [FibComp] -> Maybe String
mkFibTarget prefix path comps = ((<>) prefix . fibCompName) <$> closestParent
  where
    closestParent    = firstOf traverse parentCandidates
    parentCandidates = toListOf (traverse . filtered ((path `hasChildIn`) . fibCompSrcs)) (sortLongest comps) 
    sortLongest :: [FibComp] -> [FibComp]
    sortLongest = sortBy (comparing $ Down . maximum . fmap length . fibCompSrcs)


hasChildIn :: FilePath -> [FilePath] -> Bool
hasChildIn p = anyOf folded (p `isParentDirOf`)
  where
    isParentDirOf :: FilePath -> FilePath -> Bool
    isParentDirOf parent child = length child' == (length $ takeWhile id ee)
      where
        parent' = splitDirectories parent
        child' = splitDirectories child
        ee = zipWith (==) parent' child'
