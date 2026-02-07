{pkgs, ...}: rec {
  boomer = pkgs.callPackage ./boomer {};
  sowon = pkgs.callPackage ./sowon.nix {};
  dfm = pkgs.callPackage ./dfm.nix {};
  r2k = pkgs.callPackage ./r2k.nix {};
  sam = pkgs.callPackage ./sam.nix {};

  tockloader = pkgs.python3Packages.callPackage ./tockloader.nix {};
  jupyterlab-vim = pkgs.python3Packages.callPackage ./jupyterlab-vim.nix {};

  ga68 = pkgs.callPackage ./gcc/ga68.nix {
    # Non-GNU/Linux OSes are currently "impure" platforms, with their libc
    # outside of the store.  Thus, GCC, GFortran, & co. must always look for files
    # in standard system directories (/usr/include, etc.)
    noSysDirs =
      pkgs.stdenv.buildPlatform.system
      != "x86_64-solaris"
      && pkgs.stdenv.buildPlatform.system != "x86_64-kfreebsd-gnu";
  };
}
