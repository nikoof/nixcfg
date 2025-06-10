{pkgs, ...}: rec {
  boomer = pkgs.callPackage ./boomer {};
  dfm = pkgs.callPackage ./dfm.nix {};
  r2k = pkgs.callPackage ./r2k.nix {};
  sam = pkgs.callPackage ./sam.nix {};

  python3Packages = {
    jupyterlab-vim = pkgs.python3Packages.callPackage ./jupyterlab-vim.nix {};
  };
}
