inputs: {
  config,
  wlib,
  lib,
  pkgs,
  ...
}: {
  imports = [wlib.wrapperModules.neovim];

  config = {
    package = pkgs.unstable.neovim-unwrapped;
    settings.config_directory = "/home/nikoof/.config/nvim";

    specMods = {
      # See https://github.com/BirdeeHub/nix-wrapper-modules/discussions/506.
      options.extraPackages = lib.mkOption {
        type = lib.types.listOf wlib.types.stringable;
        default = [];
        description = "a extraPackages spec field to put packages to suffix to the PATH";
      };
    };

    extraPackages = config.specCollect (acc: v: acc ++ (v.extraPackages or [])) [];
    info = {
      nixpkgs = "${inputs.nixpkgs}";
    };

    specs.general = {
      data = with pkgs.vimPlugins; [
        nvim-highlight-colors
        base16-nvim

        bufferline-nvim
        toggleterm-nvim
        smart-splits-nvim

        mini-align
        mini-pick
        mini-icons
        oil-nvim
        comment-nvim

        pkgs.unstable.vimPlugins.luasnip

        pkgs.unstable.vimPlugins.nvim-treesitter.withAllGrammars
      ];
    };

    # Kinda repetitive...
    specs.c = {
      enable = lib.mkDefault false;
      data = with pkgs; [unstable.vimPlugins.nvim-lspconfig];
      extraPackages = with pkgs; [gdb clang-tools];
    };

    specs.nix = {
      enable = lib.mkDefault false;
      data = with pkgs; [unstable.vimPlugins.nvim-lspconfig];
      extraPackages = with pkgs; [nil alejandra];
    };

    specs.rust = {
      enable = lib.mkDefault false;
      data = with pkgs; [unstable.vimPlugins.nvim-lspconfig];
      extraPackages = with pkgs; [rust-analyzer clippy];
    };

    specs.haskell = {
      enable = lib.mkDefault false;
      data = with pkgs; [unstable.vimPlugins.nvim-lspconfig];
      extraPackages = with pkgs; [haskell-language-server];
    };

    specs.python = {
      enable = lib.mkDefault false;
      data = with pkgs; [unstable.vimPlugins.nvim-lspconfig];
      extraPackages = with pkgs; [basedpyright];
    };

    specs.lua = {
      enable = lib.mkDefault false;
      data = with pkgs; [unstable.vimPlugins.nvim-lspconfig];
      extraPackages = with pkgs; [lua-language-server stylua];
    };

    specs.java = {
      enable = lib.mkDefault false;
      data = with pkgs; [unstable.vimPlugins.nvim-lspconfig vimPlugins.nvim-jdtls];
      extraPackages = with pkgs; [jdt-language-server];
    };

    specs.typst = {
      enable = lib.mkDefault false;
      data = with pkgs; [unstable.vimPlugins.nvim-lspconfig];
      extraPackages = with pkgs; [tinymist];
    };
  };
}
