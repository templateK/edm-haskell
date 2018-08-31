{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module CabalTarget (initialise) where

import Control.Monad.IO.Class
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
import qualified Distribution.Types.Lens    as L
import Distribution.Types.PackageName
import Data.Maybe
import System.Directory (makeAbsolute)
import System.FilePath (doprFileName)
import Data.List (\\)



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
  :: forall m s. (WithCallStack, MonadEmacs m, Monad (m s), MonadIO (m s))
  => EmacsFunction ('S ('S 'Z)) 'Z 'False s m
getCabalTarget (R cabalFilePathRef (R pathRef Stop)) = do

  pwd           <- liftIO $ getCurrentDirectory
  cabalFilePath <- fromUTF8BS <$> extractString cabalFilePathRef
  let compPath = pwd \\ dropFileName cabalFilePath

  genPkgsDesc   <- liftIO $ readGenericPackageDescription normal cabalFilePath

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
  let match = if relPrjRoot `isSubOf` libs
                then "lib:" <> gpkg
                else fromMaybe "" $ (<>) "exe:" <$> (relPrjRoot `compOf` exes)
  produceRef =<< makeString (toUTF8BS match)
  where
    -- TODO: proper subdirectory testing
    isSubOf p dirs = fromMaybe False (elemOf folded p <$> dirs)
    compOf p dirs  = fst <$> findOf folded (\xs -> elem p xs . snd) dirs
    gpkgLens = L.pkgName . to unPackageName
    libsLens = L.hsSourceDirs
    exesLens = runGetter $ (,)
                <$> Getter (L.exeName . to unUnqualComponentName)
                <*> Getter L.hsSourceDirs
