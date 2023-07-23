{ config, pkgs, nixpkgs, home-manager, ... }:

{
  imports = [];

  boot.loader = {
    systemd-boot = {
      enable = true;
      consoleMode = "max";
    };

    efi.canTouchEfiVariables = true;
  };

  networking = {
    hostName = "nkideapad";
    networkmanager.enable = true;

    firewall = {
      enable = true;
      allowedTCPPorts = [ ];
      allowedUDPPorts = [ ];
    };
  };

  time.timeZone = "Europe/Bucharest";
  i18n.defaultLocale = "en_US.UTF-8";

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nixpkgs.config.allowUnfree = true;

  services.xserver = {
    enable = true;
    displayManager.sddm = {
      enable = true;
      theme = "sugar-candy-tokyonight";
    };
    windowManager.leftwm.enable = true;
    layout = "us,ro,de";
    xkbVariant = ",std,qwerty";
    xkbOptions = "grp:win_space_toggle,compose:menu";
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
  services.printing.enable = true;
  services.udisks2.enable = true;

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
      "nkbox" = { id = "6KIED2W-IJFLOZN-BM4KOU3-HOOZFO4-MGZV6LH-Z75QBSY-C3UW73O-2GA3HQO"; };
    };
    folders = {
      "Obsidian" = {
        path = "/home/nikoof/Documents/nkbrain";
	devices = [ "nkgalaxy" "nkbox" ];
      };
      "KeePass" = {
        path = "/home/nikoof/KeePass";
	devices = [ "nkgalaxy" "nkbox" ];
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

    GTK_IM_MODULE   = "xim";
  };

  environment.systemPackages = 
    with pkgs; let localPkgs = import ../packages { inherit pkgs; }; in [
    curl
    neovim
    git
    stow
    gnupg pinentry
    localPkgs.sddm-sugar-candy-tokyonight-nixbg
    playerctl
    libsForQt5.qtstyleplugin-kvantum
    lxappearance
    pulseaudio
    cifs-utils
    hunspellDicts.en_US
    hunspellDicts.en_GB-ise
  ];

  fonts.fonts = with pkgs; [
    fira-code
    nerdfonts
    symbola
    corefonts
  ];

  users.users.nikoof = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
  };

  programs.dconf.enable = true;

  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  system.stateVersion = "23.05";
}

