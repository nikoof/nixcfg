{
  config,
  inputs,
  pkgs,
  lib,
  ...
}: let
  cfg = config.apps.util;
in {
  imports = [];

  options = {
    apps.util.cli.enable = lib.mkEnableOption "Misc cli utilities (`rg`, `fzf` etc.)";
    apps.util.media.enable = lib.mkEnableOption "Media utilities (`ffmpeg`, `mpv`, `zbar` etc.)";
    apps.util.monitoring.enable = lib.mkEnableOption "Monitoring utilities (`btop`, `nvtop` etc.)";
    apps.util.networking.enable = lib.mkEnableOption "Network utilities (`nc`, `nmap`, `dig` etc.)";
  };

  config = {
    environment.systemPackages = with pkgs;
      []
      ++ lib.lists.optionals cfg.cli.enable [
        zip
        unzip
        p7zip-rar

        ripgrep
        fzf
        q-text-as-data
        mprocs # TODO: remove if switch to tmux
      ]
      ++ lib.lists.optionals cfg.media.enable [
        ffmpeg
        mpv
        imagemagick
        zbar
        qrencode
        yt-dlp
      ]
      ++ lib.lists.optionals cfg.monitoring.enable [
        du-dust
        btop
        nvtopPackages.full
      ]
      ++ lib.lists.optionals cfg.networking.enable [
        nmap # provides ncat
        socat # nc alternative
        bind.dnsutils
        tcpdump
        wireshark
        xh
        curl
      ];
  };
}
