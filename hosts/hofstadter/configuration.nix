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

  nix.settings.cores = 12;

  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/tomorrow-night.yaml";
    image = ../../wallpapers/moebius-bw.png;
    override = {
      base00 = "000000";
    };

    targets.plymouth.enable = false;
    targets.qt.enable = false;

    # INFO: these two targets add overlays to nixpkgs, which causes errors when using readOnlyPkgs.
    # However, due to how this is done in stylix, it is not possible to prevent it by simply disabling the targets.

    # targets.gnome-text-editor.enable = false;
    # targets.nixos-icons.enable = false;
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
      terminal = 14;
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
    nameservers = [
      "9.9.9.9"
      "149.122.122.122"
    ];

    tempAddresses = "default";

    firewall.enable = true;
    nftables.enable = false;
  };

  networking.firewall.allowedTCPPorts = [];
  networking.firewall.allowedUDPPorts = [];
  services.resolved = {
    enable = true;
    dnsovertls = "opportunistic";
    extraConfig = ''
      MulticastDNS=yes
    '';
  };

  environment.systemPackages = with pkgs; [
    inputs.agenix.packages."${system}".default

    man-pages
    man-pages-posix
    man-db

    nvtopPackages.full

    cifs-utils
    eduvpn-client
  ];

  networking.networkmanager = {
    enable = true;
    wifi.macAddress = "random";
    ethernet.macAddress = "preserve";
  };

  # For mount.cifs, required unless domain name resolution is not needed.
  fileSystems."/home/nikoof/remote/nkwrt/misc" = {
    device = "//10.10.0.1/misc";
    fsType = "cifs";
    options = let
      # this line prevents hanging on network split
      automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
    in ["${automount_opts},credentials=${config.age.secrets.smb-fw2b.path},uid=1000,gid=100"];
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

  wm.xmonad.enable = true;

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

  hardware.opentabletdriver = {
    enable = true;
    daemon.enable = true;
  };

  # peripherals.wacom.enable = true;
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

  programs.virt-manager.enable = true;
  virtualisation.spiceUSBRedirection.enable = true;
  virtualisation.libvirtd = {
    enable = true;
    qemu = {
      package = pkgs.qemu_kvm;
      runAsRoot = true;
      swtpm.enable = true;
      ovmf = {
        enable = true;
        packages = [
          (pkgs.OVMF.override {
            secureBoot = true;
            tpmSupport = true;
          })
          .fd
        ];
      };
    };
  };
  virtualisation.docker = {
    enable = true;
  };
}
