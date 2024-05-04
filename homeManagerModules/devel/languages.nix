{
  pkgs,
  inputs,
  config,
  lib,
  ...
}: {
  options = {
    devel.languages.cpp.enable = lib.mkEnableOption "Enable C/C++ development tools";
    devel.languages.rust.enable = lib.mkEnableOption "Enable Rust development tools";
    devel.languages.python.enable = lib.mkEnableOption "Enable Python development tools";
    devel.languages.haskell.enable = lib.mkEnableOption "Enable Haskell development tools";
    devel.languages.nix.enable = lib.mkEnableOption "Enable Nix development tools";
  };

  config = {
    home.packages = with pkgs;
    with config.devel.languages;
      lib.lists.optionals nix.enable [
        nil
      ]
      ++ lib.lists.optionals cpp.enable [
        clang
        clang-tools
      ]
      ++ lib.lists.optionals rust.enable [
        cargo
        rustc
        rust-analyzer
      ]
      ++ lib.lists.optionals python.enable [
        python311
        pyright
      ]
      ++ lib.lists.optionals haskell.enable [
        ghc
        cabal-install
        haskell-language-server
      ];
  };
}
