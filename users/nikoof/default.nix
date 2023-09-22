{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./alacritty.nix
    ./bash.nix
    ./starship.nix
    ./zathura.nix
  ];

  home.username = "nikoof";
  home.homeDirectory = "/home/nikoof";

  home.packages = with pkgs; [
    xclip
    exa
    neofetch
    taskwarrior
    discord
    betterdiscordctl
    unstable.firefox-bin
    unstable.thunderbird-bin
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
    virt-manager
    local.lunar-client
    unstable.zoom-us
    unstable.ciscoPacketTracer8
  ];

  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = ["qemu:///system"];
      uris = ["qemu:///system"];
    };
  };

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

  xresources.extraConfig = ''
    Sxiv.foreground: #e5e9f0
    Sxiv.background: #2e3440
  '';

  home.stateVersion = "23.05";
}
