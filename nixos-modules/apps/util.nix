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
    programs.wireshark = lib.mkIf cfg.networking.enable {
      enable = true;
      dumpcap.enable = true;
    };

    environment.systemPackages = with pkgs;
      []
      ++ lib.lists.optionals cfg.cli.enable [
        zip
        unzip
        p7zip-rar

        file
        exiftool

        ripgrep
        fzf
        q-text-as-data
        mprocs # TODO: remove if switch to tmux
        jq
      ]
      ++ lib.lists.optionals cfg.media.enable [
        ffmpeg
        mpv
        imagemagick
        pdftk
        zbar
        qrencode
        yt-dlp
      ]
      ++ lib.lists.optionals cfg.monitoring.enable [
        dust
        btop
        nvtopPackages.full
      ]
      ++ lib.lists.optionals cfg.networking.enable [
        nmap # provides ncat
        unstable.gost # better socat
        bind.dnsutils
        tcpdump
        wireshark
        termshark
        xh
        curl
        wireguard-tools
      ];
  };
}
