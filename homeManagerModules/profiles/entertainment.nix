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
    profiles.entertainment.enable = lib.mkEnableOption "Entertainment and social media";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      mpv
      spotify
      unstable.discord
      betterdiscordctl
    ];
  };
}
