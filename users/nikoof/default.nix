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
      xclip
      btop
      du-dust

      # Development
      neovide
      rust-analyzer
      clang
      clang-tools
      python311
      pyright
      ghc
      cabal-install
      haskell-language-server
      nil
      virt-manager

      # School
      unstable.zoom-us
      unstable.ciscoPacketTracer8

      # Media
      mpv
      ffmpeg_5-full
      spotify
      gimp
      libreoffice-qt
      qbittorrent
      kdenlive
      tenacity
      obs-studio

      # Gaming
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
