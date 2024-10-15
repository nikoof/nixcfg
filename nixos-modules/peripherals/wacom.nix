{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  cfg = config.peripherals.wacom;
in {
  options.peripherals.wacom = {
    enable = lib.mkEnableOption "Enable Wacom tablet support";
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      services.xserver.wacom.enable = true;
    })
    (lib.mkIf config.desktop.plasma.enable {
      environment.systemPackages = with pkgs; [
        kdePackages.wacomtablet
      ];
    })
  ];
}
