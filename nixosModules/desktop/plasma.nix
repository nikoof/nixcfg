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
  };
}
