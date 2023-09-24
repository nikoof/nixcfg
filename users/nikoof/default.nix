{
  config,
  inputs,
  pkgs,
  ...
}: {
  home-manager.useGlobalPkgs = true;
  home-manager.users.nikoof = {
    imports = [
      inputs.nixvim.homeManagerModules.nixvim

      ./alacritty.nix
      ./bash.nix
      ./nvim.nix
      ./starship.nix
      ./zathura.nix
    ];

    home.username = "nikoof";
    home.homeDirectory = "/home/nikoof";

    programs.nixvim.enable = true;

    home.packages = with pkgs; [
      # Utilities
      neofetch
      xclip
      btop
      du-dust

      # Development
      virt-manager
      gh
      vscode

      # School
      unstable.zoom-us
      unstable.ciscoPacketTracer8

      # Media
      mpv
      spotify
      gimp
      libreoffice
      qbittorrent
      syncplay

      # Gaming
      heroic
      bottles
      local.lunar-client

      # Productivity
      taskwarrior
      obsidian
      keepassxc

      # Messaging
      discord
      betterdiscordctl
      unstable.firefox-bin
      unstable.thunderbird-bin
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

    programs.exa = {
      enable = true;
      enableAliases = true;
      extraOptions = [
        "--group-directories-first"
        "--header"
      ];
      git = true;
      icons = true;
    };

    dconf.settings = {
      "org/virt-manager/virt-manager/connections" = {
        autoconnect = ["qemu:///system"];
        uris = ["qemu:///system"];
      };
    };

    home.stateVersion = "23.05";
  };
}
