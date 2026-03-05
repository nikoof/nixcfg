{
  pkgs,
  inputs,
  config,
  lib,
  ...
}: let
  cfg = config.devel.languages;
in {
  options.devel.languages = {
    algol68.enable = lib.mkEnableOption "Enable Algol68 tools";
    cpp.enable = lib.mkEnableOption "Enable C/C++ development tools";
    rust.enable = lib.mkEnableOption "Enable Rust development tools";
    python.enable = lib.mkEnableOption "Enable Python development tools";
    haskell.enable = lib.mkEnableOption "Enable Haskell development tools";
    nix.enable = lib.mkEnableOption "Enable Nix development tools";
  };

  config = {
    home.packages = with pkgs;
      lib.lists.optionals cfg.nix.enable [
        nil
      ]
      ++ lib.lists.optionals cfg.cpp.enable [
        gcc
        gdb
        clang-tools
        gnumake
      ]
      ++ lib.lists.optionals cfg.rust.enable [
        cargo
        rustc
        rust-analyzer
      ]
      ++ lib.lists.optionals cfg.python.enable
      (let
        python = python313.withPackages (pp:
          with pp; [
            requests
            types-requests
            beautifulsoup4
            types-beautifulsoup4
          ]);
      in [
        python
        basedpyright
      ])
      ++ lib.lists.optionals cfg.haskell.enable [
        ghc
        cabal-install
        haskell-language-server
      ]
      ++ lib.lists.optionals cfg.algol68.enable [
        unstable.local.ga68
        unstable.local.ga68.info
      ];
  };
}
