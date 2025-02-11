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
  home.stateVersion = "24.05";

  wm.xmonad.enable = true;

  programs.btop.enable = true;
  programs.fzf.enable = true;
  programs.autorandr.enable = true;

  apps = {
    alacritty.enable = true;
    zathura.enable = true;
    # taskwarrior.enable = true;
    tmux.enable = true;
    nvim.enable = true;
  };

  shell = {
    enable = true;
    bash.enable = true;
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
    cpp.enable = false;
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

  home.packages = with pkgs; let
    tex = texlive.combine {
      inherit
        (texlive)
        scheme-medium
        biblatex
        typewriter
        pgfplots
        flagderiv
        lipsum
        import
        esint
        ;
    };
  in [
    # Uni stuff
    # mathematica # this is now packaged as a unified app that is not in nixpkgs yet

    chromium
    libreoffice-qt6-still
    obsidian
    graphviz
    zotero
    rnote
    thunderbird

    easyeffects
    helvum

    unstable.spotify
    vesktop

    mpv
    ffmpeg_6-full
    obs-studio
    gimp
    unstable.qbittorrent

    anki-bin
    biber
    dot2tex
    tex

    local.sam

    unstable.ciscoPacketTracer8
    neomutt
  ];
}
