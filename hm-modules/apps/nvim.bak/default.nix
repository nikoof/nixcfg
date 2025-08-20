{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  cfg = config.apps.nvim;
  utils = inputs.nixCats.utils;
in {
  imports = [
    inputs.nixCats.homeModule
  ];

  options = {
    apps.nvim.enable = lib.mkEnableOption "Enable nvim";
  };

  config = lib.mkIf cfg.enable {
    nixCats = {
      enable = true;
      luaPath = ./.;
      packageNames = ["nvim"];
      categoryDefinitions.replace = {
        pkgs,
        settings,
        categories,
        extra,
        name,
        mkPlugin,
        ...
      } @ packageDef: {
        lspsAndRuntimeDeps = {};
        startupPlugins = {};
        optionalPlugins = {};
      };

      packageDefinitions.replace = {
        nvim = {
          pkgs,
          name,
          mkPlugin,
          ...
        }: {
          settings = {
            suffix-path = true;
            suffix-LD = true;
            # see :help nixCats.flake.outputs.settings
            # IMPORTANT:
            # your aliases may not conflict with other packages.
            # aliases = [ "vim" ];
          };
          # and a set of categories that you want
          # All categories you wish to include must be marked true
          categories = {
            # general = true;
          };
          # anything else to pass and grab in lua with `nixCats.extra`
          extra = {};
        };
      };
    };
  };
}
