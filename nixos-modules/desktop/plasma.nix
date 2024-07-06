{
  config,
  inputs,
  pkgs,
  lib,
  ...
}: let
  cfg = config.desktop.plasma;
in {
  imports = [./common.nix];

  options.desktop.plasma = {
    enable = lib.mkEnableOption "Enable Plasma";
  };

  config = lib.mkIf cfg.enable {
    desktop.common.redshift.enable = lib.mkDefault true;
    services.xserver = {
      enable = true;
      displayManager.sddm.enable = true;
      desktopManager.plasma6.enable = true;
    };

    environment.plasma6.excludePackages = with pkgs;
    with kdePackages; [
      kate
      elisa
      okular
      konsole
    ];

    environment.systemPackages = with pkgs;
    with kdePackages; [
      kcalc
      kdeconnect-kde

      sddm-kcm
      redshift-plasma-applet

      libsForQt5.polonium
    ];
  };
}
