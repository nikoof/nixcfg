{ pkgs, ... }:

{
  home.username = "nikoof";
  home.homeDirectory = "/home/nikoof";

  home.packages = with pkgs; [
    polybar picom dunst feh
    dmenu rofi j4-dmenu-desktop
    xclip scrot
    exa neofetch
    taskwarrior
    discord betterdiscordctl
    firefox thunderbird
    spotify
    keepassxc
    qbittorrent
    themechanger
    gimp
    libreoffice
    dracula-icon-theme
    heroic
    bottles
    sxiv
    obsidian
  ];

  programs.rofi = {
    enable = true;
    font = "FiraCode Nerd Font Mono 12";
    theme = ./tokyonight.rasi;
    extraConfig = {
      modi = "run,drun,ssh";
      show-icons = true;
      terminal = "alacritty";
      icon-theme = "Dracula";
      display-drun = "   Apps ";
      display-run = "   Run ";
      case-sensitive = false;
      parse-hosts = true;
      parse-known-hosts = true;
    };
  };
  
  programs.alacritty = {
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
  };

  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    settings = {
      format = pkgs.lib.concatStrings [
        "[∴ ](bold blue)"
	      "$hostname"
	      "$directory"
	      "([∴](bold blue) ($c) )"
	      "([∴](bold blue) ($rust) )"
	      "([∴](bold blue) ($python))"
	      " $fill "
	      ''(\[$cmd_duration\])''
	      "$line_break"
	      ''\[$username\]''
	      " $character"
	      "[λ](bold blue) "
      ];

      username = {
        style_user = "white bold";
        style_root = "red bold";
        format = "[$user]($style)";
        disabled = false;
        show_always = true;
	    };

	    directory = {
        truncation_length = 4;
	      style = "blue bold";
	      read_only = " ";
	    };

	    hostname = {
        ssh_only = false;
        ssh_symbol = "";
        style = "bold green";
        format = "[@](bold blue)[$hostname]($style)[ ∴ ](bold blue)";
	    };

      character = {
        format = "$symbol ";
        success_symbol = "[⟶](bold green)";
        error_symbol = "[⟶](bold red)";
	    };

      cmd_duration = {
        min_time = 500;
        format = "[$duration]($style)";
        style = "bold yellow";
      };

      fill = { symbol = " "; };

      c = {
        symbol = "";
        format = "[$symbol $name-$version]($style)";
        style = "bold";
        version_format = "$raw";
      };
      
      rust = {
        symbol = "";
        format = "[$symbol $version]($style)";
        style = "red bold";
      };
      
      python = {
        symbol = "";
        format = ''[$symbol$pyenv_prefix ($version) (\($virtualenv\))]($style)'';
        style = "yellow bold";
      };
    };
  };

  programs.zathura = {
    enable = true;
    options = {
      font = "FiraCode Nerd Font Mono 10";
      default-fg = "#c0caf5";
      default-bg = "#24283b";
      inputbar-fg = "#c0caf5";
      inputbar-bg = "#24283b";
      statusbar-fg = "#c0caf5";
      statusbar-bg = "#1a1b26";
      notification-fg = "#c0caf5";
      notification-bg = "#24283b";
      notification-error-fg = "#24283b";
      notification-error-bg = "#f7768e";
      notification-warning-fg = "#24283b";
      notification-warning-bg = "#e0af68";
      completion-fg = "#c0caf5";
      completion-bg = "#24283b";
      completion-group-fg = "#c0caf5";
      completion-group-bg = "#1a1b26";
      completion-highlight-fg = "#24283b";
      completion-highlight-bg = "#bb9af7";
      highlight-color = "#7aa2f7";
      highlight-active-color = "#bb9af7";
      highlight-fg = "#1a1b26";
      recolor = true;
      recolor-keephue = true;
      recolor-darkcolor = "#c0caf5";
      recolor-lightcolor = "#1a1b26";
      recolor-reverse-video = true;
      render-loading = true;
      render-loading-fg = "#c0caf5";
      render-loading-bg = "#24283b";
      selection-clipboard = "clipboard";
      selection-notification = false;
    };
  };

  xresources.extraConfig = ''
    Sxiv.foreground: #c0caf5
    Sxiv.background: #24283b
  '';

  home.stateVersion = "23.05";
}
