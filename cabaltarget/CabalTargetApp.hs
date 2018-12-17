module Main where


import Data.ByteString.Char8 (pack, unpack)
import UI.Butcher.Monadic.IO
import UI.Butcher.Monadic.Command
import UI.Butcher.Monadic.Pretty

import CabalTarget (getCabalTarget)


main :: IO ()
main = mainFromCmdParserWithHelpDesc $ \helpDesc -> do

  addCmdSynopsis "a program help you find cabal target string."
  addCmdHelpStr "example command: cabal-target find foo.cabal src/Bar.hs"

  addCmd "version" $ do
    addCmdHelpStr "prints the version of this program"
    addCmdImpl $ putStrLn "0.1.0.0"


  addCmd "help" $ addCmdImpl $ print $ ppHelpShallow helpDesc

  cablaPath  <- addParamString "CABAL-FILE"
    (paramHelpStr "project cabal configuration file path.  eg) foo.cabal")
  hsFilePath <- addParamString "HS-FILE"
    (paramHelpStr "haskell source file path. eg) src/Bar.hs")

  addCmdImpl $ serachCabalTarget cablaPath hsFilePath


serachCabalTarget :: String -> String -> IO ()
serachCabalTarget cabalPath hsFilePath =
  getCabalTarget (pack cabalPath) (pack hsFilePath)
  >>= maybe (return ()) (print . unpack)
