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

  programs.bash.enable = true;

  gtk = {
    enable = true;

    theme = {
      name = "Tokyonight-Moon-B";
      package = pkgs.tokyo-night-gtk; 
    };

    # TODO: Change overall theme to Rose Pine
    iconTheme = {
      name = "oomox-rose-pine-moon";
      package = pkgs.rose-pine-icon-theme;
    };

    cursorTheme = {
      name = "Simp1e-Tokyo-Night-Storm";
      package = pkgs.simp1e-cursors;
    };

    font = {
      name = "Sans Regular";
      size = 11;
    };

    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = true;
    };
    gtk4.extraConfig = {
      gtk-application-prefer-dark-theme = true;
    };
  };
  home.sessionVariables.GTK_THEME = "Tokyonight-Moon-B";

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      gtk-theme = "Tokyonight-Moon-B";
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
