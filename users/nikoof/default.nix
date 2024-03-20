{
  config,
  inputs,
  pkgs,
  ...
}: {
  home-manager.useGlobalPkgs = true;
  home-manager.users.nikoof = {
    imports = [
      # inputs.nixvim.homeManagerModules.nixvim

      ./alacritty.nix
      ./bash.nix
      ./git.nix
      # ./nvim.nix
      ./nushell.nix
      ./starship.nix
      ./taskwarrior.nix
      ./tmux.nix
      ./zathura.nix
    ];

    home.username = "nikoof";
    home.homeDirectory = "/home/nikoof";

    home.sessionVariables = {
      EDITOR = "nvim";
    };

    home.packages = with pkgs; [
      # Utilities
      neofetch
      xclip
      btop
      du-dust

      # Development
      virt-manager
      neovide
      vscode
      clang
      python311
      rust-analyzer
      clang-tools
      pyright
      ghc
      cabal-install
      haskell-language-server
      nil

      # School
      unstable.zoom-us
      unstable.ciscoPacketTracer8

      # Media
      mpv
      spotify
      gimp
      inkscape
      libreoffice-qt
      qbittorrent
      syncplay
      brasero
      kdenlive
      tenacity

      # Gaming
      heroic
      bottles
      local.lunar-client

      # Productivity
      taskwarrior
      obsidian
      keepassxc
      rnote

      # Messaging
      unstable.discord
      betterdiscordctl
      firefox-bin
      chromium

      # Theming
      nordic
      nordzy-icon-theme
      simp1e-cursors

      ffmpeg_5-full
      obs-studio
    ];

    programs.direnv.enable = true;

    programs.helix = {
      enable = true;
      settings = {
        theme = "nord";
        editor = {
          line-number = "relative";
        };
      };
    };

    programs.bat = {
      enable = true;
      config.theme = "Nord";
    };

    home.stateVersion = "23.05";
  };
}
