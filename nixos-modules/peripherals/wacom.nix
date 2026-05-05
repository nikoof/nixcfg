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
    enable = lib.mkEnableOption "Enable Wacom tablet support (currently through opentabletdriver).";
  };

  config = lib.mkIf cfg.enable {
    # Works better with stuff like osu!lazer.
    hardware.opentabletdriver = {
      enable = true;
      daemon.enable = true;
    };
  };
}
