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
  boot.extraModulePackages = with config.boot.kernelPackages; [ xone ];

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
    nvidiaSettings = true;

    prime = {
      intelBusId = "PCI:0:2:0";
      nvidiaBusId = "PCI:1:0:0";
    };
  };

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      vaapiVdpau
    ];
  };

  hardware.bluetooth.enable = true;

  systemd.targets.machines.enable = true;
  systemd.nspawn."ubuntu-jammy" = {
    enable = true;
    execConfig = {
      Boot = true;
      ResolvConf = "bind-host";
    };
    networkConfig = {
      Private = false;
    };
  };
  systemd.services."systemd-nspawn@ubuntu-jammy" = {
    enable = true;
    overrideStrategy = "asDropin";
    wantedBy = [ "machines.target" ];
  };
  
  services.udev.extraRules = ''
    SUBSYSTEM=="usb", ATTR{idVendor}=="2e8a", ATTR{idProduct}=="000c", MODE:="666"
  '';

  services.xserver = {
    enable = true;
    videoDrivers = [ "nvidia" ];
    displayManager.sddm = {
      enable = true;
    };
    desktopManager.plasma5.enable = true;
  };

  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  virtualisation.libvirtd.enable = true;

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
      "nkbox" = { id = "6KIED2W-IJFLOZN-BM4KOU3-HOOZFO4-MGZV6LH-Z75QBSY-C3UW73O-2GA3HQO"; };
      "nkgalaxy" = { id = "FY2JIBO-6VYRLZD-YJBAUSF-W5CMUV7-RCXYVMU-NAKKIHT-NNZLTHA-ZHV3SAE"; };
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

  services.thermald.enable = true;
  services.auto-cpufreq = {
    enable = true;
    settings = {
      battery = {
         governor = "powersave";
         turbo = "never";
      };
      charger = {
         governor = "performance";
         turbo = "auto";
      };
    };
  };

  environment.systemPackages = with pkgs; with libsForQt5; [
    libthai
    pyocd
    picoprobe-udev-rules
    libusb1
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
    pciutils
    usbutils
    hunspellDicts.en_US
    hunspellDicts.en_GB-ise
    local.nord-sddm-theme
    acpi
    lm_sensors

    kde-gtk-config
    kcalc

    qemu
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
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "dialout" "tty" "plugdev" "libvirtd" ];
  };

  programs.gamemode.enable = true;
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
  };

  system.stateVersion = "23.05";
}

