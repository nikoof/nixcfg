{ pkgs ? (import <nixpkgs>) {}
, ...
}:

{
  sddm-sugar-candy-tokyonight = pkgs.libsForQt5.callPackage ./sddm-sugar-candy-tokyonight.nix { };
  sddm-sugar-candy-tokyonight-nixbg = pkgs.libsForQt5.callPackage ./sddm-sugar-candy-tokyonight-nixbg.nix { };
  nordic = pkgs.callPackage ./nordic.nix { };
}
