{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [
    inputs.self.outputs.homeManagerModules.default
    inputs.agenix.homeManagerModules.default
    ../../../secrets
  ];

  # ------------------[General]-------------------
  home.username = "nikoof";
  home.homeDirectory = "/home/nikoof";
  home.preferXdgDirectories = true;
  home.stateVersion = "24.05";

  home.sessionPath = [
    "$HOME/.local/bin"
  ];

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
    algol68.enable = true;
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
    zathura.enable = false;
    sioyek.enable = true;
    tmux.enable = true;
    nvim.enable = true;
  };

  programs.nnn.enable = true;
  programs.btop.enable = true;
  programs.fzf.enable = true;

  programs.taskwarrior = {
    enable = true;
    package = pkgs.taskwarrior3;
    config = {
      sync.server = {
        url = "https://tw.gw.nikoof.ro";
        client_id = "d24fc42c-857c-48cd-ab3d-943c56f6eb42";
      };
    };
  };

  home.packages = with pkgs; [
    tlaplus18

    # Terminal apps
    uutils-coreutils-noprefix
    presenterm
    tokei

    newsboat

    # CAD
    freecad
    kicad
    orca-slicer

    # Typesetting
    graphviz
    unstable.typst

    sageWithDoc

    # Productivity
    anki-bin
    obsidian
    zotero
    rnote
    restream

    # Literally browsers but x4
    ungoogled-chromium
    libreoffice-fresh
    thunderbird
    unstable.spotify
    local.coppelia

    # IM
    discord # browsers +1
    unstable.signal-desktop

    # Audio stuff (?)
    helvum

    # Images/Video
    gimp3
    obs-studio

    # Misc
    local.sam
    unstable.osu-lazer-bin
    unstable.qbittorrent
    rclone
    rclone-browser

    sdrpp

    unstable.vintagestory
  ];
}
