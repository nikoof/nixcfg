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
      ]
      ++ lib.lists.optionals cfg.rust.enable [
        cargo
        rustc
        rust-analyzer
      ]
      ++ lib.lists.optionals cfg.python.enable [
        python311
        pyright
      ]
      ++ lib.lists.optionals cfg.haskell.enable [
        ghc
        cabal-install
        haskell-language-server
      ];
  };
}
