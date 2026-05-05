{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.apps.alacritty;
in {
  options.apps.alacritty = {
    enable = lib.mkEnableOption "Enable Alacritty";
  };

  config = lib.mkIf cfg.enable {
    programs.alacritty = {
      enable = true;
      settings = {
        window = {
          padding = {
            x = 3;
            y = 3;
          };
          dynamic_padding = true;

          title = "Alacritty";
          dynamic_title = true;

          class = {
            instance = "Alacritty";
            general = "Alacritty";
          };
        };

        scrolling = {
          history = 10000;
          multiplier = 3;
        };

        bell = {
          animation = "EaseOutExpo";
          duration = 0;
        };

        selection.save_to_clipboard = true;

        cursor = {
          style = {
            shape = "Underline";
            blinking = "On";
          };

          blink_interval = 750;
          unfocused_hollow = true;
          thickness = 0.15;
        };
      };
    };
  };
}
