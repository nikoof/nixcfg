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
    mpv
    btop
    du-dust
    syncplay
    nordic
  ];

  programs.bash.enable = true;

  qt.style.name = "kvantum";

  gtk = {
    enable = true;

    gtk2.extraConfig = ''
      gtk-application-prefer-dark-theme = 1
      gtk-theme-name = "Nordic"
    '';

    theme = {
      name = "Graphite-Dark-nord";
      package = pkgs.graphite-gtk-theme; 
    };

    iconTheme = {
      name = "Nordzy";
      package = pkgs.nordzy-icon-theme;
    };

    cursorTheme = {
      name = "Simp1e-Nord-Dark";
      package = pkgs.simp1e-cursors;
    };

    font = {
      name = "Sans Regular";
      size = 11;
    };
  };

  programs.rofi = import ./rofi.nix;
  programs.alacritty = import ./alacritty.nix;
  programs.starship = import ./starship.nix { inherit pkgs; };
  programs.zathura = import ./zathura.nix;

  xresources.extraConfig = ''
    Sxiv.foreground: #e5e9f0
    Sxiv.background: #2e3440
  '';

  home.stateVersion = "23.05";
}
