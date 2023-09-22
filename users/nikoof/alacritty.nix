{
  config,
  pkgs,
  ...
}: {
  programs.alacritty = {
    enable = true;
    settings = {
      colors = {
        primary = {
          background = "#2e3440";
          foreground = "#d8dee9";
          dim_foreground = "#a5abb6";
        };
        cursor = {
          text = "#2e3440";
          cursor = "#d8dee9";
        };
        vi_mode_cursor = {
          text = "#2e3440";
          cursor = "#d8dee9";
        };
        selection = {
          text = "CellForeground";
          background = "#4c566a";
        };
        search = {
          matches = {
            foreground = "CellBackground";
            background = "#88c0d0";
          };
          footer_bar = {
            background = "#434c5e";
            foreground = "#d8dee9";
          };
        };
        normal = {
          black = "#3b4252";
          red = "#bf616a";
          green = "#a3be8c";
          yellow = "#ebcb8b";
          blue = "#81a1c1";
          magenta = "#b48ead";
          cyan = "#88c0d0";
          white = "#e5e9f0";
        };
        bright = {
          black = "#4c566a";
          red = "#bf616a";
          green = "#a3be8c";
          yellow = "#ebcb8b";
          blue = "#81a1c1";
          magenta = "#b48ead";
          cyan = "#8fbcbb";
          white = "#eceff4";
        };
        dim = {
          black = "#373e4d";
          red = "#94545d";
          green = "#809575";
          yellow = "#b29e75";
          blue = "#68809a";
          magenta = "#8c738c";
          cyan = "#6d96a5";
          white = "#aeb3bb";
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

        gtk_theme_variant = null;
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

      draw_bold_text_with_bright_colors = true;

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
}
