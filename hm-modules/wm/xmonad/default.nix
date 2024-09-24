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
    home.packages = with pkgs; [
      scrot
      mpv
      dmenu
      trayer

      zip
      unzip
      unrar

      alsa-utils
      brightnessctl
    ];

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
      };
    };
  };
}
