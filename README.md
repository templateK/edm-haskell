# edm-haskell
A emacs dynamic module by haskell.
  - Returns the cabal target of the given file path.
  - Fast fuzzy matching score calculation with parallel haskell runtime.

## requirements
Emacs must be compiled with option --with-modules
```
git clone <emacs-repo> <emacs local repo>
cd <emacs local repo>
./configure --with-modules ...
```

## installing
Put following elisp code in your emacs configuration.
### macOS
```
(module-load "absolute path to the libedm-haskell.dylib")
```
### Linux
```
(module-load "absolute path to the libedm-haskell.so")
```
### Windows
```
(module-load "absolute path to the edm-haskell.dll")
```

## Usage

### Calling Function
```
(edm-haskell-cabal-target "absolute cabal file path" "absolute path of the target file")
```
Absolute cabal file path can be obtained by calling function `haskell-cabal-find-file` from haskell-mode.
and you can get absolute target file path by calling function `buffer-file-name`.

```
(edm-haskell-score-matches "niddle" '("haystack1" "haystack2" "haystack3"))
```
`niddle` is search target and `haystack#` is candidates of the search.


### Return Value
 - `edm-haskell-cabal-target`
    If this function cannot find cabal target, it just returns `nil`.
    Otherwise, returns string which can be used by cabal commands.for example `lib:foo`, `exe:bar`

 - `edm-haskell-score-matches`
    If `haystacks` is nil then returns nil, and the search target is `nil` then it throws exception.


## Attribution
  - `FuzzyMatch` Module is the awesome work which is based on [emacs-native][1]
 
 
[1]: https://github.com/sergv/emacs-native.git
