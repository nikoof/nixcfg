{ config, pkgs, ... }:

{
  home.username = "nikoof";
  home.homeDirectory = "/home/nikoof";

  home.packages = with pkgs; [
    polybar picom dunst feh
    dmenu rofi
    xclip scrot
    exa neofetch
    taskwarrior
    discord betterdiscordctl
    firefox unstable.thunderbird
    spotify
    keepassxc
    qbittorrent
    gimp
    libreoffice
    heroic
    bottles
    sxiv
    obsidian
  ];

  gtk = {
    enable = true;

    theme = {
      name = "Nordic";
      package = pkgs.nordic; 
    };

    iconTheme = {
      name = "Nordzy-turquoise-dark";
      package = pkgs.nordzy-icon-theme;
    };

    cursorTheme = {
      name = "Nordzy-cursors-white";
      package = pkgs.nordzy-cursor-theme;
    };

    font = {
      name = "Sans Regular";
      size = 11;
    };

    gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = true;
    };
    gtk4.extraConfig = {
      gtk-application-prefer-dark-theme = true;
    };
  };
  home.sessionVariables.GTK_THEME = "Nordic";

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      gtk-theme = "Nordic";
      color-scheme = "prefer-dark";
    };
  };

  programs.rofi = import ./rofi.nix;
  programs.alacritty = import ./alacritty.nix;
  programs.starship = import ./starship.nix { inherit pkgs; };
  programs.zathura = import ./zathura.nix;

  xresources.extraConfig = ''
    Sxiv.foreground: #c0caf5
    Sxiv.background: #24283b
  '';

  home.stateVersion = "23.05";
}
