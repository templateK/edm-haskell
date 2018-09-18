(defun to_root (path) (expand-file-name (concat default-directory path)))

(module-load (to_root "dist-newstyle/build/x86_64-osx/ghc-8.4.3/edm-haskell-0.1.0.0/f/edm-haskell/build/edm-haskell/libedm-haskell.dylib"))

(defun test-edm-haskell-cabal-target()
  (progn
    (print (edm-haskell-cabal-target "" ""))
    (print (edm-haskell-cabal-target (to_root "tests/samples/applied-fp-course.txt")       (to_root "exe/Main.hs")))
    (print (edm-haskell-cabal-target (to_root "tests/samples/foreign-library-cabal.txt")   (to_root "src/Lib.hs")))
    (print (edm-haskell-cabal-target (to_root "tests/samples/sample_cabal.txt")            (to_root "src/Lib.hs")))
    (print (edm-haskell-cabal-target (to_root "tests/samples/sample_multiple_library.txt") (to_root "path/to/foo/bar/Sample.hs")))
    (print (edm-haskell-cabal-target (to_root "tests/samples/sample_multiple_library.txt") (to_root "another/path/ha/Wat.hs")))))
