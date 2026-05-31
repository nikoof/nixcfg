{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  cfg = config.desktop.wm.xmonad;
in {
  options.desktop.wm.xmonad = {
    enable = lib.mkEnableOption "Enable xmonad config";
  };

  config = lib.mkIf cfg.enable {
    # WM
    services.xserver = {
      enable = true;
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };

    services.displayManager.ly = {
      enable = true;
      x11Support = true;
      settings = {
        battery_id = "BAT0";
        vi_mode = true;
        vi_default_mode = "normal";
      };
    };

    environment.variables = {
      "_JAVA_AWT_WM_NONREPARENTING" = 1; # Fix AWT apps not being tiled
    };

    # Locker
    programs.i3lock = {
      enable = true;
      u2fSupport = config.peripherals.nitrokey.enable;
    };

    programs.xss-lock = {
      enable = true;
      lockerCommand = "${pkgs.i3lock}/bin/i3lock -f -k -i ${../../../../wallpapers/moebius-bw.png}";
    };

    # Utilities
    services.udisks2.enable = true;

    # Other glue
    programs.dconf.enable = true;
    xdg.portal = {
      enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-gtk
      ];
      config.common.default = "*";
    };
  };
}
