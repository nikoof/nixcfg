{pkgs, ...}: rec {
  boomer = pkgs.callPackage ./boomer {};
  dfm = pkgs.callPackage ./dfm.nix {};
  r2k = pkgs.callPackage ./r2k.nix {};
  sam = pkgs.callPackage ./sam.nix {};

  tockloader = pkgs.python3Packages.callPackage ./tockloader.nix {};
  jupyterlab-vim = pkgs.python3Packages.callPackage ./jupyterlab-vim.nix {};
}
