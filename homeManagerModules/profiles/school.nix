{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  cfg = config.profiles.media;
in {
  options = {
    profiles.school.enable = lib.mkEnableOption "Apps for school";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      unstable.zoom-us
      unstable.ciscoPacketTracer8
      libreoffice-qt
    ];
  };
}
