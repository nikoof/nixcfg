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
      dmenu-nk = pkgs.writeShellScriptBin "dmenu-nk" ''
        ${pkgs.dmenu}/bin/dmenu -fn "FiraCode Nerd Font Mono-12" \
        -nb "#000000" -nf "#ffffff" -sb "#b294bb" -sf "#1d1f21" "$@"
      '';
    in
      with pkgs; [
        trayer
        scrot

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

    services.picom.enable = true;

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

    #  base00: "#1d1f21"
    #  base01: "#282a2e"
    #  base02: "#373b41"
    #  base03: "#969896"
    #  base04: "#b4b7b4"
    #  base05: "#c5c8c6"
    #  base06: "#e0e0e0"
    #  base07: "#ffffff"
    #  base08: "#cc6666"
    #  base09: "#de935f"
    #  base0A: "#f0c674"
    #  base0B: "#b5bd68"
    #  base0C: "#8abeb7"
    #  base0D: "#81a2be"
    #  base0E: "#b294bb"
    #  base0F: "#a3685a"

    qt = {
      enable = true;
      style.name = "Adwaita-Dark";
      style.package = pkgs.adwaita-qt;
    };
  };
}
