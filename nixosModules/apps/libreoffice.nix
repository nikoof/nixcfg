{
  config,
  inputs,
  pkgs,
  lib,
  ...
}: let
  cfg = config.apps.libreoffice;
in {
  options.apps.libreoffice = {
    enable = lib.mkEnableOption "Enable libreoffice";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      libreoffice

      hunspellDicts.en_US
      hunspellDicts.en_GB-ise
      aspellDicts.ro
    ];
  };
}
