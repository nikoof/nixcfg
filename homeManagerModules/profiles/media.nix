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
    profiles.media.enable = lib.mkEnableOption "Video, audio and image editing apps; torrents";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      ffmpeg_5-full
      kdenlive
      tenacity
      obs-studio
      gimp
      qbittorrent
    ];
  };
}
