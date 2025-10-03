{
  config,
  lib,
  inputs,
  ...
}: let
  cfg = config.apps.nvim;
  utils = inputs.nixCats.utils;
  nvim = import ./nvim.nix {inherit inputs;};
in {
  imports = [
    inputs.nixCats.homeModule
  ];

  options = {
    apps.nvim.enable = lib.mkEnableOption "Enable nvim(Cats) config.";
  };

  config = lib.mkIf cfg.enable {
    home.sessionVariables = {
      EDITOR = nvim.defaultPackageName;
    };

    nixCats = {
      enable = true;
      luaPath = ./.;
      packageNames = nvim.packageNames;
      # nixpkgs_version = inputs.nixpkgs;
      # this will add the overlays from ./overlays and also,
      # add any plugins in inputs named "plugins-pluginName" to pkgs.neovimPlugins
      # It will not apply to overall system, just nixCats.
      addOverlays = [
        (utils.standardPluginOverlay inputs)
      ];

      # the .replace vs .merge options are for modules based on existing configurations,
      # they refer to how multiple categoryDefinitions get merged together by the module.
      # for useage of this section, refer to :h nixCats.flake.outputs.categories
      categoryDefinitions.replace = nvim.categoryDefinitions;
      packageDefinitions.replace = nvim.packageDefinitions;
    };
  };
}
