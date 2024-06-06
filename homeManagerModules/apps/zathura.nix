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
      options = with config.colorScheme.palette;
        {
          font = "FiraCode Nerd Font Mono 10";
          recolor = true;
          recolor-keephue = true;
          recolor-lightcolor = "#1a1b26";
          recolor-reverse-video = true;
          render-loading = true;
          selection-clipboard = "clipboard";
          selection-notification = false;
        }
        // {
          notification-error-bg = "#${base00}";
          notification-error-fg = "#${base08}";
          notification-warning-bg = "#${base00}";
          notification-warning-fg = "#${base09}";
          notification-bg = "#${base00}";
          notification-fg = "#${base04}";

          completion-bg = "#${base00}";
          completion-fg = "#${base04}";
          completion-group-bg = "#${base01}";
          completion-group-fg = "#${base04}";
          completion-highlight-bg = "#${base0C}";
          completion-highlight-fg = "#${base01}";

          index-bg = "#${base00}";
          index-fg = "#${base07}";
          index-active-bg = "#${base07}";
          index-active-fg = "#${base00}";

          inputbar-bg = "#${base00}";
          inputbar-fg = "#${base05}";

          statusbar-bg = "#${base00}";
          statusbar-fg = "#${base05}";

          highlight-color = "#${base09}";
          highlight-active-color = "#${base08}";

          default-bg = "#${base00}";
          default-fg = "#${base04}";
          render-loading = "true";
          render-loading-bg = "#${base00}";
          render-loading-fg = "#${base02}";

          recolor-lightcolor = "#${base00}";
          recolor-darkcolor = "#${base06}";
        };
    };
  };
}
