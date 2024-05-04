{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  options = {
    hardware.wacom.enable = lib.mkEnableOption "Enable Wacom tablet support";
  };

  config = lib.mkIf config.hardware.wacom.enable {
    services.xserver.wacom.enable = true;
    environment.systemPackages = with pkgs; [
      wacomtablet
    ];
  };
}
