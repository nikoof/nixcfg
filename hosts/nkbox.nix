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
    xkbOptions = "grp:win_space_toggle,compose:menu";
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

    GTK_IM_MODULE   = "xim";
  };

  environment.systemPackages = 
    with pkgs; let localPkgs = import ../packages { inherit pkgs; }; in [
    curl
    neovim
    git
    stow
    gnupg pinentry
    localPkgs.sddm-sugar-candy-tokyonight
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

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
  };

  programs.dconf.enable = true;

  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  system.stateVersion = "23.05";
}

