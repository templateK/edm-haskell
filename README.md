# emacs-dyn-cabal
A emacs dynamic module.
Provides function which returns the cabal target of the given file path.

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
(module-load "absolute path to the libemacs-dyn-cabal.dylib")
```
### Linux
```
(module-load "absolute path to the libemacs-dyn-cabal.so")
```

## Usage

### Calling Function
```
(emacs-dyn-cabal-target "absolute cabal file path" "absolute path of the target file")
```

### Return Value
If this function cannot find cabal target, it just returns empty string.<br>
Otherwise returns string which can be used by cabal commands.<br>
for example `lib:foo, exe:bar`