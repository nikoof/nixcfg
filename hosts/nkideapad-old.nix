{ config, pkgs, localPkgs, home-manager, ... }:

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
    hostName = "nkideapad-old";
    networkmanager.enable = true;

    firewall = {
      enable = true;
      allowedTCPPorts = [ ];
      allowedUDPPorts = [ ];
    };
  };

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

  services.xserver = {
    enable = true;
    displayManager.sddm = {
      enable = true;
      theme = "sugar-candy-tokyonight";
    };
    windowManager.leftwm.enable = true;
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

  environment.systemPackages = with pkgs; [
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

  system.stateVersion = "23.05";
}

