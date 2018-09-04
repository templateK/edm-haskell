(defun to_root (path) (expand-file-name (concat default-directory path)))

(module-load (to_root "dist/build/emacs-dyn-cabal/libemacs-dyn-cabal.so"))

(defun test-emacs-cabal-target()
  (progn
    (print (emacs-dyn-cabal-target (to_root "tests/samples/applied-fp-course.txt")       (to_root "exe/Main.hs")))
    (print (emacs-dyn-cabal-target (to_root "tests/samples/foreign-library-cabal.txt")   (to_root "src/Lib.hs")))
    (print (emacs-dyn-cabal-target (to_root "tests/samples/sample_cabal.txt")            (to_root "src/Lib.hs")))
    (print (emacs-dyn-cabal-target (to_root "tests/samples/sample_multiple_library.txt") (to_root "path/to/foo/bar/Sample.hs")))
    (print (emacs-dyn-cabal-target (to_root "tests/samples/sample_multiple_library.txt") (to_root "another/path/ha/Wat.hs")))))
