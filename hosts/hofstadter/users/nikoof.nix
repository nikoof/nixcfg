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

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
  };

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

  programs.nnn = {
    enable = true;
  };

  programs.ncspot = {
    enable = true;
    settings = {
      use_nerdfont = true;
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

    brainrot = pkgs.writeShellScriptBin "brainrot" ''
      FILE="/home/nikoof/videos/memes/1-hour-brainrot.webm"
      if [ ! -z $1 ]; then
        FILE=$1
      fi
      DUR=$(${pkgs.ffmpeg_6-full}/bin/ffmpeg -i $FILE 2>&1 | ${pkgs.ripgrep}/bin/rg -i DURATION | tail -1)
      HOUR=$(echo $DUR | cut -d ':' -f2)
      MINUTE=$(echo $DUR | cut -d ':' -f3)
      SECOND=$(echo $DUR | cut -d ':' -f4 | cut -d '.' -f1)
      LEN=$((HOUR * 60 * 60 + MINUTE * 60 + SECOND))
      R=$(shuf -i 0-$LEN -n1)

      echo "# mpv EDL v0" > /tmp/brainrot.edl
      echo "$FILE,$R,$LEN" >> /tmp/brainrot.edl
      echo "$FILE,0,$R" >> /tmp/brainrot.edl

      ${pkgs.mpv}/bin/mpv --loop /tmp/brainrot.edl
    '';
  in [
    # Uni stuff
    # mathematica # this is now packaged as a unified app that is not in nixpkgs yet

    logisim-evolution # 2IC30

    chromium
    libreoffice-qt6-still
    obsidian
    graphviz
    zotero
    rnote
    thunderbird

    easyeffects
    helvum

    brainrot

    # vesktop
    discord
    signal-desktop
    iamb

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

    asm-lsp
  ];
}
