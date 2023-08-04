{ inputs, config, pkgs, lib, home-manager, ... }:

{
  imports = [];
  boot.loader = {
    systemd-boot = {
      enable = true;
      consoleMode = "max";
    };

    efi.canTouchEfiVariables = true;
  };

  boot.plymouth = {
    enable = true;
    themePackages = with pkgs; [ nixos-bgrt-plymouth ];
    theme = "nixos-bgrt";
  };

  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking = {
    hostName = "nkideapad";
    networkmanager = {
      enable = true;
    };

    useDHCP = lib.mkDefault true;

    firewall = {
      enable = true;
      allowedTCPPorts = [ 1630 1631 1632 1633 1634 1635 1636 1637 1638 1639 1640 1641 ];
      allowedUDPPorts = [ 1630 1631 1632 1633 1634 1635 1636 1637 1638 1639 1640 1641 ];
    };
  };

  hardware.nvidia = {
    modesetting.enable = true;
    open = true;
    nvidiaSettings = true;
  };

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      vaapiVdpau
    ];
  };

  services.xserver = {
    enable = true;
    videoDrivers = [ "nvidia" ];
    displayManager.sddm = {
      enable = true;
      theme = "Nord";
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
      "nkideapad" = { id = "TJJMOB6-T5YQQCE-HLRVVPW-NV5RWES-CYHWQQH-E25WYYF-VY4P6C4-KU66BA5"; };
    };
    folders = {
      "Obsidian" = {
        path = "/home/nikoof/Documents/nkbrain";
	devices = [ "nkgalaxy" "nkideapad" ];
      };
      "KeePass" = {
        path = "/home/nikoof/KeePass";
	devices = [ "nkgalaxy" "nkideapad" ];
      };
    };
  };

  environment.systemPackages = with pkgs; [
    curl
    neovim
    git
    stow
    gnupg
    pinentry
    playerctl
    lxappearance
    pulseaudio
    cifs-utils
    hunspellDicts.en_US
    hunspellDicts.en_GB-ise
    libsForQt5.qtstyleplugin-kvantum
    libsForQt5.plasma-framework
    local.nord-sddm-theme
  ];

  fonts.fonts = with pkgs; [
    fira-code
    nerdfonts
    symbola
    corefonts
    noto-fonts
  ];

  users.users.nikoof = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
  };

  programs.gamemode.enable = true;
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
  };

  system.stateVersion = "23.05";
}

