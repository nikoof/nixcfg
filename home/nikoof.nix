{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [
    inputs.self.outputs.homeManagerModules.default
  ];

  home.username = "nikoof";
  home.homeDirectory = "/home/nikoof";
  home.preferXdgDirectories = true;

  programs.btop.enable = true;

  apps = {
    alacritty.enable = true;
    zathura.enable = true;
    taskwarrior.enable = true;
    tmux.enable = true;
    nvim.enable = true;
  };

  shell = {
    bash.enable = true;
    nushell.enable = true;
    starship.enable = true;
  };

  devel.git = {
    enable = true;
    signing = true;
    github.enable = true;
  };

  programs.direnv.enable = true;
  devel.languages = {
    cpp.enable = true;
    rust.enable = true;
    haskell.enable = true;
    nix.enable = true;
    python.enable = true;
  };

  home.packages = with pkgs; [
    pueue

    virt-manager

    chromium

    unstable.zoom-us
    unstable.ciscoPacketTracer8
    libreoffice-qt6-still

    obsidian
    rnote

    mpv
    unstable.spotify
    webcord
    betterdiscordctl
    unstable.discord

    ffmpeg_5-full
    kdenlive
    tenacity
    obs-studio
    gimp
    qbittorrent
  ];

  home.stateVersion = "23.05";
}
