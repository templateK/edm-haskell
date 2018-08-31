{ mkDerivation, base, bytestring, Cabal, containers, emacs-module
, formatting, lens, megaparsec, mtl, random, stdenv, text, time
, vector
}:
mkDerivation {
  pname = "emacs-dyn-cabal";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring Cabal containers emacs-module formatting lens
    megaparsec mtl random text time vector
  ];
  executableHaskellDepends = [
    base bytestring Cabal containers emacs-module formatting lens
    megaparsec mtl random text time vector
  ];
  license = stdenv.lib.licenses.bsd3;
}
