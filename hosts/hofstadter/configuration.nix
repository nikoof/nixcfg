{
  inputs,
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
    inputs.nixos-hardware.nixosModules.common-gpu-nvidia

    inputs.lanzaboote.nixosModules.lanzaboote

    ./hardware.nix
    ./containers/arch.nix
    ./containers/ubuntu.nix
  ];

  nix.settings.cores = 16;

  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/tomorrow-night.yaml";
    image = ../../wallpapers/moebius-bw.png;
    override = {
      base00 = "000000";
    };

    targets.plymouth.enable = false;
  };

  stylix.opacity = {
    applications = 1.0;
    terminal = 0.8;
    desktop = 1.0;
    popups = 1.0;
  };

  stylix.fonts = {
    sizes = {
      applications = 10;
      terminal = 12;
      desktop = 10;
      popups = 10;
    };

    monospace = {
      package = pkgs.nerd-fonts.fira-code;
      name = "FiraCode Nerd Font Mono";
    };

    sansSerif = {
      package = pkgs.dejavu_fonts;
      name = "DejaVu Sans";
    };

    serif = {
      package = pkgs.dejavu_fonts;
      name = "DejaVu Serif";
    };
  };

  system.stateVersion = "24.05";
  nixpkgs.hostPlatform = "x86_64-linux";

  boot.loader = {
    systemd-boot.enable = lib.mkForce false;
    systemd-boot.consoleMode = "max";
    efi.canTouchEfiVariables = true;
  };

  boot.lanzaboote = {
    enable = true;
    pkiBundle = "/etc/secureboot";
  };

  boot.plymouth = {
    enable = true;
    theme = "cubes";
    themePackages = with pkgs; [
      (adi1090x-plymouth-themes.override {
        selected_themes = ["cubes"];
      })
    ];
  };

  hardware.bluetooth.enable = true;
  networking = {
    hostName = "hofstadter";
    tempAddresses = "disabled";
    firewall.enable = true;
    nftables.enable = true;
  };

  networking.firewall.allowedTCPPorts = [47121 51820];
  networking.firewall.allowedUDPPorts = [67 47121 51820];

  environment.systemPackages = with pkgs; [
    man-pages
    man-pages-posix
    man-db
    # nvtopPackages.full
    cifs-utils
    eduvpn-client

    python312Packages.rns
    python312Packages.nomadnet
    # local.python3Packages.nomadnet

    virt-viewer
    virt-manager
  ];

  networking.networkmanager = {
    enable = true;
  };

  # For mount.cifs, required unless domain name resolution is not needed.
  fileSystems."/home/nikoof/remote/nkwrt/misc" = {
    device = "//10.10.0.1/misc";
    fsType = "cifs";
    options = let
      # this line prevents hanging on network split
      automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
    in ["${automount_opts},credentials=/etc/nixos/smb-secrets,uid=1000,gid=100"];
  };

  users.users.nikoof = {
    description = "Nicolas Bratoveanu";
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager" "dialout" "tty" "plugdev" "uucd" "libvirtd" "optical" "cdrom" "ubridge" "adbusers" "kvm" "docker"];
  };

  home-manager = {
    extraSpecialArgs = {inherit inputs;};
    useGlobalPkgs = true;

    users.nikoof = ./users/nikoof.nix;
  };

  services.xserver = {
    enable = true;
    displayManager.lightdm = {
      enable = true;
      greeters.enso.enable = true;
    };
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
  };

  services.autorandr = {
    enable = false; # DISABLED
    profiles = let
      fingerprint = {
        DP-1-1 = "00ffffffffffff004c2d2c0d46415a420b1e010380341d782a5295a556549d250e5054bb8c00b30081c0810081809500a9c001010101023a801871382d40582c450009252100001e000000fd0032481e5111000a202020202020000000fc00433234463339300a2020202020000000ff0048345a4e3330393033370a2020017202031af14690041f131203230907078301000066030c00100080011d00bc52d01e20b828554009252100001e8c0ad090204031200c4055000925210000188c0ad08a20e02d10103e96000925210000180000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c9";
        DP-1-2 = "00ffffffffffff004c2d2c0d46415a420b1e010380341d782a5295a556549d250e5054bb8c00b30081c0810081809500a9c001010101023a801871382d40582c450009252100001e000000fd0032481e5111000a202020202020000000fc00433234463339300a2020202020000000ff0048345a4e3330383232370a2020017202031af14690041f131203230907078301000066030c00100080011d00bc52d01e20b828554009252100001e8c0ad090204031200c4055000925210000188c0ad08a20e02d10103e96000925210000180000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c9";
        eDP-1 = "00ffffffffffff0006afa33d00000000001f010495221378036e8593585892281e505400000001010101010101010101010101010101963780c8703826406c30aa0058c2100000180f2580c8703826406c30aa0058c21000001800000000000000000000000000000000000000000002001040ff0f3c7d0f13287d2020200049";
      };
    in {
      "mobile" = {
        inherit fingerprint;
        config = {
          eDP-1 = {
            enable = true;
            crtc = 0;
            mode = "1920x1080";
            position = "0x0";
            primary = true;
            rate = "60.04";
          };
        };
      };
      "docked" = {
        inherit fingerprint;
        config = {
          eDP1.enable = false;
          DP-1-2 = {
            enable = true;
            crtc = 0;
            mode = "1920x1080";
            position = "0x0";
            primary = true;
            rate = "60.00";
          };
          DP-1-1 = {
            enable = true;
            crtc = 2;
            mode = "1920x1080";
            position = "1920x0";
            rate = "60.00";
          };
        };
      };
      # "triple" = {
      #   inherit fingerprint;
      #   config = {
      #     inherit eDP-1 DP-1-1 DP-1-2;
      #   };
      # };
    };
  };

  desktop = {
    xdgDirs.enable = true;

    printing.enable = true;
    printing.autodetect = true;
  };

  apps.gns3.enable = true;
  apps.gaming.steam.enable = true;

  peripherals.wacom.enable = true;
  peripherals.nitrokey = {
    enable = true;
    enableSSHSupport = true;
  };

  services.ollama = {
    enable = true;
    acceleration = "cuda";
    package = pkgs.unstable.ollama-cuda;
  };

  services.openssh.enable = true;

  services.syncthing = {
    enable = true;
    user = "nikoof";
    dataDir = "/home/nikoof/Sync";
    configDir = "/home/nikoof/.config/syncthing";
  };

  services.chrony = {
    enable = true;
  };

  systemd.oomd = {
    enable = true;
  };

  services.protonmail-bridge.enable = true;
  services.gnome.gnome-keyring.enable = true;

  programs.bash = {
    completion.enable = true;
    enableLsColors = true;
  };

  programs.dconf.enable = true;

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
    ];
    config.common.default = "*";
  };

  services.udev = {
    # BBC micro:bit for Tock development
    packages = with pkgs; [
      android-udev-rules
    ];
    extraRules = ''
      ACTION!="add|change", GOTO="openocd_rules_end"
      SUBSYSTEM!="usb|tty|hidraw", GOTO="openocd_rules_end"

      ATTRS{product}=="*CMSIS-DAP*", MODE="664", GROUP="plugdev"

      LABEL="openocd_rules_end"
    '';
  };

  virtualisation.libvirtd = {
    enable = true;
  };
  virtualisation.docker = {
    enable = true;
  };
}
