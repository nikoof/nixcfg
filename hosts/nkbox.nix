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

  networking = {
    hostName = "nkbox";
    networkmanager = {
      enable = true;
    };

    useDHCP = lib.mkDefault true;
    tempAddresses = "disabled";

    firewall = {
      enable = true;
      allowedTCPPortRanges = [
        { from = 1630; to = 1641; }
	{ from = 1714; to = 1764; }
      ];
      allowedUDPPortRanges = [
        { from = 1630; to = 1641; }
	{ from = 1714; to = 1764; }
      ];
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
    };
    desktopManager.plasma5.enable = true;
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
      "nkideapad" = { id = "DFBQIQO-4Q5RHSF-TFQAH2X-7IH7URS-EQDBRHT-VAK7HAY-WXQC75W-7SOMIAO"; };
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
    libsForQt5.kdeconnect-kde
  ];

  fonts.fonts = with pkgs; [
    fira-code
    nerdfonts
    corefonts
    noto-fonts
    noto-fonts-emoji
    noto-fonts-cjk-sans
  ];

  users.users.nikoof = {
    description = "Nicolas Bratoveanu";
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

