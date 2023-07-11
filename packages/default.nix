{ pkgs, ... }:

{
  sddm-sugar-candy-tokyonight = pkgs.libsForQt5.callPackage ./sddm-sugar-candy-tokyonight.nix { };
}
