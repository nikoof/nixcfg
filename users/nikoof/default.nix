{ pkgs, ... }:

{
  home.username = "nikoof";
  home.homeDirectory = "/home/nikoof";

  home.packages = with pkgs; [
    polybar picom dunst feh
    dmenu rofi j4-dmenu-desktop
    xclip scrot
    exa neofetch
    taskwarrior
    discord betterdiscordctl
    firefox unstable.thunderbird
    spotify
    keepassxc
    qbittorrent
    themechanger
    gimp
    libreoffice
    dracula-icon-theme
    heroic
    bottles
    sxiv
    obsidian
  ];

    enable = true;

    };





    };
  };

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
