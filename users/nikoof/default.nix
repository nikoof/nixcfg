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

  qt.style.name = "kvantum";
  gtk = import ./theme.nix;

  programs.bash = import ./bash.nix;
  programs.rofi = import ./rofi.nix;
  programs.alacritty = import ./alacritty.nix;
  programs.starship = import ./starship.nix { inherit pkgs; };
  programs.zathura = import ./zathura.nix;

  programs.bat = {
    enable = true;
    config.theme = "Nord-sublime";
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

  xresources.extraConfig = ''
    Sxiv.foreground: #e5e9f0
    Sxiv.background: #2e3440
  '';

  home.stateVersion = "23.05";
}
