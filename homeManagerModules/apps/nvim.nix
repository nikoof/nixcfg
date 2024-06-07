{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  cfg = config.apps.nvim;
in {
  imports = [];

  options = {
    apps.nvim.enable = lib.mkEnableOption "Enable nvim";
  };

  config = lib.mkIf cfg.enable {
    home.sessionVariables = {
      EDITOR = "nvim";
    };

    home.packages = with pkgs; [
      neovim
      neovide
    ];
  };
}
