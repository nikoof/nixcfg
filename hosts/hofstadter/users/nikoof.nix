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
  programs.fzf.enable = true;
  programs.autorandr.enable = true;

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
    signing = false;
    github.enable = true;
    lazygit.enable = true;
  };

  programs.direnv.enable = true;
  devel.languages = {
    cpp.enable = true;
    rust.enable = true;
    haskell.enable = true;
    nix.enable = true;
    python.enable = true;
  };

  programs.vscode = {
    enable = true;
    package =
      pkgs.vscode.fhsWithPackages
      (ps:
        with ps; [
          rustup
          zlib
          openssl
          pkg-config
          icu
          dotnetCorePackages.runtime_8_0
          openjdk
        ]);

    userSettings = {
      "editor.fontLigatures" = true;
      "workbench.iconTheme" = "file-icons";
    };
  };

  home.packages = with pkgs; [
    # Uni stuff
    # mathematica # this is now packaged as a unified app that is not in nixpkgs yet

    chromium
    libreoffice-qt6-still
    obsidian
    rnote

    unstable.spotify
    unstable.discord

    mpv
    ffmpeg_5-full
    obs-studio
    gimp
    qbittorrent

    anki-bin

    local.sam
  ];

  wm.xmonad.enable = true;

  home.stateVersion = "24.05";
}
