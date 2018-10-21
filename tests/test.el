(require 'cl-lib)
(require 'subr-x)


(defun abspath-filename-with-shell-find (filename)
   (string-trim (shell-command-to-string (concat "find `pwd` -name \"" filename "\" -type f"))))

(module-load (abspath-filename-with-shell-find "libedm-haskell.dylib"))


(defun to-root (path) (expand-file-name (concat default-directory path)))

(cl-defstruct testcase number cabal-sample hs-source expected)

(setq testcase-params-alist
      `(,(make-testcase
          :number        1
          :cabal-sample  ""
          :hs-source     ""
          :expected      nil
          )
        ,(make-testcase
          :number        2
          :cabal-sample  nil
          :hs-source     nil
          :expected      nil
          )
        ,(make-testcase
          :number        3
          :cabal-sample  "tests/samples/applied-fp-course.txt"
          :hs-source     "exe/Main.hs"
          :expected      "exe:level01-exe"
          )
        ,(make-testcase
          :number        4
          :cabal-sample  "tests/samples/foreign-library-cabal.txt"
          :hs-source     "src/Lib.hs"
          :expected      "lib:emacs-dyn-cabal"
          )
        ,(make-testcase
          :number        5
          :cabal-sample  "tests/samples/sample_cabal.txt"
          :hs-source     "src/Lib.hs"
          :expected      "lib:kurl"
          )
        ,(make-testcase
          :number        6
          :cabal-sample  "tests/samples/sample_multiple_library.txt"
          :hs-source     "path/to/foo/bar/Sample.hs"
          :expected      "lib:foo"
          )
        ,(make-testcase
          :number        7
          :cabal-sample  "tests/samples/sample_multiple_library.txt"
          :hs-source     "another/path/ha/Wat.hs"
          :expected      "lib:foo"
          )
        ,(make-testcase
          :number        8
          :cabal-sample  "tests/samples/cardano-sl-explorer.txt"
          :hs-source     "bench/Main.hs"
          :expected      "bench:cardano-explorer-bench"
          )
        ,(make-testcase
          :number        9
          :cabal-sample  "tests/samples/cardano-sl-explorer.txt"
          :hs-source     "bench/Bench/Pos/Explorer/ServerBench.hs"
          :expected      "bench:cardano-explorer-bench"
          )
        ,(make-testcase
          :number        10
          :cabal-sample  "tests/samples/cardano-sl-explorer.txt"
          :hs-source     "ben/Bench/Pos/Explorer/ServerBench.hs"
          :expected      nil
          ))
) ;; end of test cases

(defun test-single-case (test-param)
  (edm-haskell-cabal-target
   (to-root (testcase-cabal-sample test-param))
   (to-root (testcase-hs-source test-param))))

(defun test-param-formatter (test-param output)
  (concat
   (if (equal (testcase-expected test-param) output)
       (format "case #%d        : %s\n" (testcase-number test-param) "SUCCESS")
     (concat
      "---------------------------------------------------------------------------\n"
      (format "case #%d        : %s\n" (testcase-number test-param) "FAILURE")
      (format "cabal-sample   : %s\n" (testcase-cabal-sample test-param ))
      (format "hs-source-file : %s\n" (testcase-hs-source    test-param ))
      (format "expected       : %s\n" (testcase-expected     test-param ))
      (format "output         : %s\n" output                             )
      "---------------------------------------------------------------------------\n"))))

(defun test-edm-haskell-cabal-target ()
  (cl-loop for param in testcase-params-alist
           do (princ (test-param-formatter param (test-single-case param)))))
