{
  config,
  pkgs,
  lib,
  ...
}: {
  options = {
    terminal.enable = lib.mkEnableOption "Enable terminal";
  };

  config = lib.mkIf config.terminal.enable {
    programs.alacritty = {
      enable = true;
      settings = {
        colors = with config.colorScheme.palette; {
          draw_bold_text_with_bright_colors = true;

          primary = {
            background = "#${base00}";
            foreground = "#${base04}";
          };
          cursor = {
            text = "#${base00}";
            cursor = "#${base04}";
          };
          vi_mode_cursor = {
            text = "#${base00}";
            cursor = "#${base04}";
          };
          selection = {
            text = "CellForeground";
            background = "#${base03}";
          };
          search = {
            matches = {
              foreground = "CellBackground";
              background = "#${base0C}";
            };
          };
          normal = {
            black = "#${base01}";
            red = "#${base08}";
            green = "#${base0B}";
            yellow = "#${base0A}";
            blue = "#${base0D}";
            magenta = "#${base0E}";
            cyan = "#${base0C}";
            white = "#${base05}";
          };
          bright = {
            black = "#${base03}";
            red = "#${base08}";
            green = "#${base0B}";
            yellow = "#${base0A}";
            blue = "#${base0D}";
            magenta = "#${base0E}";
            cyan = "#${base07}";
            white = "#${base06}";
          };
        };

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

        font = rec {
          size = 12;
          normal = {
            family = "FiraCode Nerd Font";
            style = "Regular";
          };

          bold = {
            inherit (normal) family;
            style = "Bold";
          };

          italic = {
            inherit (normal) family;
            style = "Italic";
          };

          bold_italic = {
            inherit (normal) family;
            style = "Bold Italic";
          };
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
