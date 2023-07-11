{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    <home-manager/nixos>
  ];

  boot.loader = {
    systemd-boot = {
      enable = true;
      consoleMode = "max";
    };

    efi.canTouchEfiVariables = true;
  };

  networking = {
    hostName = "nkbox";
    networkmanager.enable = true;

    firewall = {
      enable = true;
      allowedTCPPorts = [ ];
      allowedUDPPorts = [ ];
    };
  };

  time.timeZone = "Europe/Bucharest";
  i18n.defaultLocale = "en_US.UTF-8";

  hardware.nvidia = {
    modesetting.enable = true;
    open = true;
    nvidiaSettings = true;
  };

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nixpkgs.config.allowUnfree = true;

  services.xserver = {
    enable = true;
    videoDrivers = [ "nvidia" ];
    displayManager.sddm = {
      enable = true;
      theme = "sugar-candy-tokyonight";
    };
    windowManager.leftwm.enable = true;
    layout = "us,ro,de";
    xkbVariant = ",std,qwerty";
    xkbOptions = "grp:win_space_toggle";
    xrandrHeads = [
      {
        output = "DP-3";
	primary = true;
      }
      {
        output = "HDMI-0";
	monitorConfig = ''
          Option "RightOf" "DP-3"
	'';
      }
    ];
  };

  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  services.openssh.enable = true;
  services.printing = {
    enable = true;
    drivers = with pkgs; [
      postscript-lexmark
    ];
  };
  services.avahi = {
    enable = true;
    nssmdns = true;
    openFirewall = true;
  };

  services.syncthing = {
    enable = true;
    user = "nikoof";
    dataDir = "/home/nikoof/Sync";
    configDir = "/home/nikoof/.config/syncthing";
    overrideDevices = true;
    overrideFolders = true;
    devices = {
      "nkgalaxy" = { id = "FY2JIBO-6VYRLZD-YJBAUSF-W5CMUV7-RCXYVMU-NAKKIHT-NNZLTHA-ZHV3SAE"; };
    };
    folders = {
      "Obsidian" = {
        path = "/home/nikoof/Documents/nkbrain";
	devices = [ "nkgalaxy" ];
      };
      "KeePass" = {
        path = "/home/nikoof/KeePass";
	devices = [ "nkgalaxy" ];
      };
    };
  };

  environment.sessionVariables = rec {
    XDG_DATA_HOME   = "$HOME/.local/share";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_STATE_HOME  = "$HOME/.local/state";
    XDG_CACHE_HOME  = "$HOME/.cache";

    XDG_BIN_HOME    = "$HOME/.local/state";
    PATH = [ "${XDG_BIN_HOME}" ];

    RUSTUP_HOME     = "${XDG_DATA_HOME}/rustup";
    GTK2_RC_FILES   = "${XDG_CONFIG_HOME}/gtk-2.0/gtkrc";
    HISTFILE        = "${XDG_STATE_HOME}/bash/history";
    RANDFILE        = "${XDG_STATE_HOME}/rnd";
    CUDA_CACHE_PATH = "${XDG_CACHE_HOME}/nv";
    XCOMPOSECACHE   = "${XDG_CACHE_HOME}/X11/xcompose";
    SQLITE_HISTORY  = "${XDG_CACHE_HOME}/sqlite_history";
    STACK_XDG       = "1";
  };

  environment.systemPackages = with pkgs; [
    curl
    neovim
    git
    stow
    gnupg pinentry
    libsForQt5.qt5.qtgraphicaleffects
    (import ./sddm-sugar-candy-tokyonight.nix)
    playerctl
    libsForQt5.qtstyleplugin-kvantum
    lxappearance
    pulseaudio
  ];

  fonts.fonts = with pkgs; [
    fira-code
    nerdfonts
    symbola
  ];

  users.users.nikoof = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
    packages = with pkgs; [
      polybar
      picom
      dunst
      dmenu
      feh
      j4-dmenu-desktop
      xclip scrot
      starship
      alacritty
      exa
      neofetch
      taskwarrior
      discord betterdiscordctl
      firefox
      thunderbird
      spotify
      keepassxc
      qbittorrent
      themechanger
      gimp
      libreoffice
      dracula-icon-theme
      heroic
      bottles
      zathura
      sxiv
      gh
      obsidian
    ];
  };

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
  };

  programs.dconf.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}

