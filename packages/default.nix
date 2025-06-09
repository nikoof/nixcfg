{pkgs, ...}: rec {
  sam = pkgs.callPackage ./sam.nix {};
  r2k = pkgs.callPackage ./r2k.nix {};
  coq-waterproof = pkgs.coqPackages_8_17.callPackage ./coq-waterproof.nix {};
  dfm = pkgs.callPackage ./dfm.nix {};
  boomer = pkgs.callPackage ./boomer {};

  python3Packages = rec {
    jupyterlab-vim = pkgs.python3Packages.callPackage ./jupyterlab-vim.nix {};
    nomadnet = pkgs.unstable.python312Packages.callPackage ./nomadnet.nix {};
  };
}
