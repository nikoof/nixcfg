{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  cfg = config.wm.xmonad;
in {
  options.wm.xmonad = {
    enable = lib.mkEnableOption "Enable xmonad environment";
  };

  config = lib.mkIf cfg.enable {
    home.packages = let
      dmenu-nk = with config.lib.stylix.colors;
        pkgs.writeShellScriptBin "dmenu-nk" ''
          ${pkgs.dmenu}/bin/dmenu -fn "FiraCode Nerd Font Mono-12" \
          -nb "#${base00}" -nf "#${base07}" -sb "#${base0E}" -sf "#${base00}" "$@"
        '';
    in
      with pkgs; [
        trayer

        scrot
        xcolor
        xdragon

        mpv
        sxiv
        zbar

        dmenu
        dmenu-nk
        dmenu-bluetooth
        networkmanager_dmenu
        bemoji

        zip
        unzip
        unrar

        alsa-utils
        brightnessctl
      ];

    home.sessionVariables = {
      "_JAVA_AWT_WM_NONREPARENTING" = 1;
    };

    programs.xmobar = {
      enable = true;
      extraConfig = builtins.readFile ./xmobarrc;
    };

    xsession.windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad.hs;
    };

    services.dunst = {
      enable = true;
      iconTheme = config.gtk.iconTheme;
      settings = {
        urgency_low.frame_color = lib.mkForce "#${config.lib.stylix.colors.base02}";
        global = {
          origin = "top-right";
          offset = "2x25";
          frame_width = 3;
          gap_size = 3;
          progress_bar = true;
          font = lib.mkForce "FiraCode Nerd Font Mono 10";
        };
      };
    };

    services.picom = {
      enable = true;
      backend = "glx";
      shadowExclude = [
        "class_g = 'firefox' && argb"
        "window_type *= 'menu'"
        "class_g ~= 'xdg-desktop-portal' && _NET_FRAME_EXTENTS@:c && window_type = 'dialog'"
        "class_g ~= 'xdg-desktop-portal' && window_type = 'menu'"
        "_NET_FRAME_EXTENTS@:c && WM_WINDOW_ROLE@:s = 'Popup'"
        "class_i = 'Firefox' && window_type = 'utility'"
        "class_i = 'Firefox' && window_type = 'popup_menu'"
        "class_i = 'Thunderbird' && window_type = 'utility'"
        "class_i = 'Thunderbird' && window_type = 'popup_menu'"
      ];

      settings.blur-background-exclude = [
        "class_g ~= 'xdg-desktop-portal' && _NET_FRAME_EXTENTS@:c && window_type = 'dialog'"
        "class_g ~= 'xdg-desktop-portal' && window_type = 'menu'"
        "_NET_FRAME_EXTENTS@:c && WM_WINDOW_ROLE@:s = 'Popup'"
        "class_i = 'Firefox' && window_type = 'utility'"
        "class_i = 'Firefox' && window_type = 'popup_menu'"
        "class_i = 'Thunderbird' && window_type = 'utility'"
        "class_i = 'Thunderbird' && window_type = 'popup_menu'"
      ];

      settings = {
        wintypes = {
          menu = {
            shadow = false;
            blur-background = false;
          };
        };
        vsync = true;
        blur = {
          background = true;
          method = "dual_kawase";
        };
      };
    };

    xsession = {
      enable = true;
      initExtra = ''
        xset r rate 200 50
        setxkbmap -option caps:swapescape
      '';
    };

    services.xscreensaver = {
      enable = true;
      settings = {
        fadeTicks = 20;
        mode = "blank";
        timeout = "00:15:00";
        lock = true;
        lockTimeout = "00:05:00";
        dpmsEnabled = true;
        dpmsStandby = "00:30:00";
        dpmsSuspend = "00:45:00";
        dpmsOff = "01:00:00";
        dialogTheme = "Borderless Black";
      };
    };

    xresources.properties = {
      "xscreensaver-auth.borderlessblack.Dialog.headingFont" = "-*-firacode nerd font mono-bold-r-*-*-12-*-*-*-*-*-*-*";
      "xscreensaver-auth.borderlessblack.Dialog.bodyFont" = "-*-firacode nerd font mono-medium-r-*-*-12-*-*-*-*-*-*-*";
      "xscreensaver-auth.borderlessblack.Dialog.labelFont" = "-*-firacode nerd font mono-medium-r-*-*-12-*-*-*-*-*-*-*";
      "xscreensaver-auth.borderlessblack.Dialog.unameFont" = "-*-firacode nerd font mono-medium-r-*-*-12-*-*-*-*-*-*-*";
      "xscreensaver-auth.borderlessblack.Dialog.buttonFont" = "-*-firacode nerd font mono-bold-r-*-*-12-*-*-*-*-*-*-*";
      "xscreensaver-auth.borderlessblack.Dialog.dateFont" = "-*-firacode nerd font mono-medium-r-*-*-12-*-*-*-*-*-*-*";
      "xscreensaver-auth.borderlessblack.passwd.passwdFont" = "-*-firacode nerd font mono-bold-r-*-*-12-*-*-*-*-*-*-*";
      "xscreensaver-auth.dateFormat" = "%H:%M, %Y-%m-%d (%a)";
    };

    gtk = {
      enable = true;
      iconTheme = {
        package = pkgs.papirus-icon-theme;
        name = "Papirus";
      };
    };

    qt = {
      enable = true;
      style.name = "Adwaita-Dark";
      style.package = pkgs.adwaita-qt;
    };
  };
}
