{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [
    inputs.self.outputs.homeManagerModules.default
    inputs.nix-colors.homeManagerModules.default
  ];

  home.username = "nikoof";
  home.homeDirectory = "/home/nikoof";

  colorScheme = inputs.nix-colors.colorSchemes.nord;

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
    virt-manager

    unstable.zoom-us
    unstable.ciscoPacketTracer8
    # libreoffice

    obsidian
    rnote

    mpv
    spotify
    unstable.discord
    betterdiscordctl

    ffmpeg_5-full
    kdenlive
    tenacity
    obs-studio
    gimp
    qbittorrent
  ];

  home.stateVersion = "23.05";
}
