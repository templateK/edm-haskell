
pkgs: new: old: with pkgs.haskell.lib; {

# package overrides goes here

 emacs-module = new.callCabal2nix "emacs-module" (pkgs.fetchFromGitHub {
   owner  = "sergv";
   repo   = "emacs-module";
   rev    = "cee3dff20f6860904688fe90d8f897e9783f39b7";
   sha256 = "0aw0qnb12wk6cbpxn20d4ry170ag5p6vld5l2ry133mqfghv0d6a";
 }) {};

# end of package overrides
}
