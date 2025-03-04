{pkgs, ...}: {
  sam = pkgs.callPackage ./sam.nix {};
  r2k = pkgs.callPackage ./r2k.nix {};
  coq-waterproof = pkgs.coqPackages_8_17.callPackage ./coq-waterproof.nix {};
  dfm = pkgs.callPackage ./dfm.nix {};

  python3Packages = {
    jupyterlab-vim = pkgs.python3Packages.callPackage ./jupyterlab-vim.nix {};
  };
}
