{
  config,
  inputs,
  pkgs,
  lib,
  ...
}: {
  services.xserver = {
    enable = true;
    displayManager.sddm.enable = true;
    desktopManager.plasma5.enable = true;
  };

  location.provider = "geoclue2";
  services.redshift = {
    enable = true;
    executable = "/bin/redshift";
  };

  environment.systemPackages = with pkgs; [
    kde-gtk-config
    kcalc
    kdeconnect
    redshift-plasma-applet
    libsForQt5.sddm-kcm
  ];
}
