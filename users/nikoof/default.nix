{ config, pkgs , ... }:

{
  home.username = "nikoof";
  home.homeDirectory = "/home/nikoof";

  home.packages = with pkgs; [
    xclip
    exa
    neofetch
    taskwarrior
    discord
    betterdiscordctl
    firefox
    unstable.thunderbird
    chromium
    spotify
    keepassxc
    qbittorrent
    gimp
    libreoffice
    heroic
    bottles
    obsidian
    mpv
    btop
    du-dust
    syncplay
    nordic
    nordzy-icon-theme
    simp1e-cursors
    vscode
    gh
    local.lunar-client
  ];

  # qt.style.name = "kvantum";
  # gtk = import ./theme.nix { inherit pkgs; };

  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = ["qemu:///system"];
      uris = ["qemu:///system"];
    };
  };

  programs.bash = import ./bash.nix;
  programs.rofi = import ./rofi.nix;
  programs.alacritty = import ./alacritty.nix;
  programs.starship = import ./starship.nix { inherit pkgs; };
  programs.zathura = import ./zathura.nix;

  programs.direnv.enable = true;

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

  xresources.extraConfig = ''
    Sxiv.foreground: #e5e9f0
    Sxiv.background: #2e3440
  '';

  home.stateVersion = "23.05";
}
