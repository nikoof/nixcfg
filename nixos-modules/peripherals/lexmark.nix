{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  cfg = config.peripherals.lexmark;
in {
  options = {
    peripherals.lexmark.enable = lib.mkEnableOption "Enable Lexmark printer.";
  };

  config = lib.mkIf cfg.enable {
    services.printing = {
      enable = true;
      drivers = with pkgs; [
        postscript-lexmark
        lexmark-aex
      ];
    };
  };
}
