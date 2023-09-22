{pkgs ? (import <nixpkgs>) {}, ...}: {
  lunar-client = pkgs.callPackage ./lunar-client.nix {};
}
