{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  options = {
    desktop.printing.enable = lib.mkEnableOption "Enable printing support";
    desktop.printing.autodetect = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Use avahi to autodetect printers on the local network";
    };
  };

  config = lib.mkIf config.desktop.printing.enable {
    services.printing.enable = true;
    services.avahi = lib.mkIf config.desktop.printing.autodetect {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };
  };
}
