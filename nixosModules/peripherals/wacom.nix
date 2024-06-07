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

  config = lib.mkIf cfg.enable {
    services.xserver.wacom.enable = true;
    environment.systemPackages = with pkgs; [
      wacomtablet
    ];
  };
}
