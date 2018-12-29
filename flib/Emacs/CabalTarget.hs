{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE ScopedTypeVariables      #-}


module Emacs.CabalTarget (initialise) where

import qualified Data.ByteString.Char8           as C8
import           Control.Monad.IO.Class                 (MonadIO, liftIO)
import           Control.Monad.Catch                    (MonadCatch)
import           Data.Maybe                             (maybe)

import           Data.Emacs.Module.Args
import           Data.Emacs.Module.SymbolName.TH
import           Emacs.Module
import           Emacs.Module.Assert
import           Emacs.Module.Errors

import           CabalTarget                            (getCabalTarget)


initialise
  :: (WithCallStack, Throws EmacsThrow, Throws EmacsError, Throws EmacsInternalError)
  => EmacsM s ()
initialise =
  bindFunction [esym|edm-haskell-cabal-target|] =<<
    makeFunction getCabalTargetEmacs getCabalTargetDoc


getCabalTargetDoc :: C8.ByteString
getCabalTargetDoc =
  "return cabal command target from current directory."


getCabalTargetEmacs
  :: forall m s. (WithCallStack, MonadCatch (m s), MonadThrow (m s), MonadEmacs m, Monad (m s), MonadIO (m s))
  => EmacsFunction ('S ('S 'Z)) 'Z 'False s m
getCabalTargetEmacs (R cabalFilePathRef (R currentDirRef Stop)) =
  produceRef =<< maybe (intern [esym|nil|]) makeString
             =<< liftIO
             =<< getCabalTarget
              <$> extractString cabalFilePathRef
              <*> extractString currentDirRef
