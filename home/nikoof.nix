{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [
    inputs.self.outputs.homeManagerModules.default
    inputs.nix-colors.homeManagerModules.default
  ];

  home.username = "nikoof";
  home.homeDirectory = "/home/nikoof";

  colorScheme = inputs.nix-colors.colorSchemes.kanagawa;

  dconf = {
    enable = true;
    settings = {
      "org/gnome/desktop/interface" = {
        color-scheme = "prefer-dark";
      };
    };
  };

  gtk = {
    enable = true;
    theme = {
      name = "Adwaita-dark";
      package = pkgs.gnome.gnome-themes-extra;
    };

    iconTheme = {
      name = "Adwaita";
      package = pkgs.gnome.adwaita-icon-theme;
    };

    cursorTheme = {
      name = "Simp1e-Adw-Dark";
      package = pkgs.simp1e-cursors;
    };
  };

  programs.feh.enable = true;

  wm.i3.enable = true;

  wm.dwm = {
    enable = true;
    wallpaper = ./moebius.png;
  };

  apps = {
    zathura.enable = true;
    taskwarrior.enable = true;
    tmux.enable = true;
    nvim.enable = true;
  };

  terminal = {
    enable = true;
    shell.nushell.enable = true;
    shell.starship.enable = true;
  };

  devel.git = {
    enable = true;
    signing = true;
    github.enable = true;
  };

  programs.direnv.enable = true;
  devel.languages = {
    cpp.enable = true;
    rust.enable = true;
    haskell.enable = true;
    nix.enable = true;
    python.enable = true;
  };

  home.packages = with pkgs; [
    virt-manager

    unstable.zoom-us
    unstable.ciscoPacketTracer8
    # libreoffice

    obsidian
    rnote

    mpv
    spotify
    unstable.discord
    betterdiscordctl

    ffmpeg_5-full
    kdenlive
    tenacity
    obs-studio
    gimp
    qbittorrent
  ];

  home.stateVersion = "23.05";
}
