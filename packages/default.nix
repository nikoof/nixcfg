{pkgs, ...}: {
  sam = pkgs.callPackage ./sam.nix {};
  r2k = pkgs.callPackage ./r2k.nix {};
  coq-waterproof = pkgs.coqPackages_8_17.callPackage ./coq-waterproof.nix {};
}
