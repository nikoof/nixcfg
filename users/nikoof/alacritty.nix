{
  enable = true;
  settings = {
    colors = {
      primary = {
        background = "0x1a1b26";
        foreground = "0xa9b1d6";
      };

      normal = {
        black =   "0x32344a";
        red =     "0xf7768e";
        green =   "0x9ece6a";
        yellow =  "0xe0af68";
        blue =    "0x7aa2f7";
        magenta = "0xad8ee6";
        cyan =    "0x449dab";
        white =   "0x787c99";
      };

      bright = {
        black =   "0x444b6a";
        red =     "0xff7a93";
        green =   "0xb9f27c";
        yellow =  "0xff9e64";
        blue =    "0x7da6ff";
        magenta = "0xbb9af7";
        cyan =    "0x0db9d7";
        white =   "0xacb0d0";
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
}
