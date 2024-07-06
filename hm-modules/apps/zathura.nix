{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  cfg = config.apps.zathura;
in {
  options = {
    apps.zathura.enable = lib.mkEnableOption "Zathura";
  };

  config = lib.mkIf cfg.enable {
    programs.zathura = {
      enable = true;
      options = {
        font = "FiraCode Nerd Font Mono 10";
        recolor = true;
        recolor-keephue = true;
        recolor-reverse-video = true;
        render-loading = true;
        selection-clipboard = "clipboard";
        selection-notification = false;
      };
    };
  };
}
