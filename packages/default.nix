{pkgs, ...}: {
  sam = pkgs.callPackage ./sam.nix {};
}
