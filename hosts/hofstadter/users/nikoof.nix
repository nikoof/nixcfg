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

  # ------------------[General]-------------------
  home.username = "nikoof";
  home.homeDirectory = "/home/nikoof";
  home.preferXdgDirectories = true;
  home.stateVersion = "24.05";

  shell = {
    enable = true;
    bash.enable = true;
    fish.enable = true;
    starship.enable = true;
  };

  # --------------------[WM]----------------------
  wm.xmonad = {
    enable = true;
    terminal = pkgs.kitty;
  };

  # --------------------[Devel]----------------------
  devel.git = {
    enable = true;
    signing = false;
    github.enable = true;
    lazygit.enable = true;
  };

  devel.languages = {
    cpp.enable = true;
    rust.enable = true;
    haskell.enable = true;
    nix.enable = true;
    python.enable = true;
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  # --------------------[Apps]----------------------
  apps = {
    kitty.enable = true;
    zathura.enable = true;
    tmux.enable = true;
    nvim.enable = true;
  };

  programs.nnn.enable = true;
  programs.btop.enable = true;
  programs.fzf.enable = true;

  home.packages = with pkgs; [
    # Terminal apps
    uutils-coreutils-noprefix
    presenterm
    tokei

    # Typesetting
    graphviz
    typst

    # Productivity
    anki-bin
    obsidian
    zotero

    # Literally browsers but x4
    ungoogled-chromium
    libreoffice-qt6-still
    thunderbird
    unstable.spotify

    # IM
    discord # browsers +1
    gajim
    signal-desktop

    # Audio stuff (?)
    easyeffects
    helvum
    carla
    dragonfly-reverb

    # Images/Video
    gimp3
    obs-studio

    # Misc
    local.sam
    osu-lazer-bin
    unstable.qbittorrent
    rclone
    rclone-browser
  ];
}
