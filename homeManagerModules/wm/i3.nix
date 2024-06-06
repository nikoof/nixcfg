{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  cfg = config.wm.i3;
  mod = "Mod4";
in {
  imports = [];

  options.wm.i3 = {
    enable = lib.mkEnableOption "Enable i3";
  };

  config = lib.mkIf cfg.enable {
    xsession.windowManager.i3 = {
      enable = true;

      config = {
        modifier = mod;
        terminal = lib.mkDefault "alacritty";

        keybindings = lib.mkOptionDefault {
          "${mod}+h" = "focus left";
          "${mod}+j" = "focus down";
          "${mod}+k" = "focus up";
          "${mod}+l" = "focus right";

          "${mod}+Shift+h" = "move left";
          "${mod}+Shift+j" = "move down";
          "${mod}+Shift+k" = "move up";
          "${mod}+Shift+l" = "move right";
        };
      };
    };
  };
}
