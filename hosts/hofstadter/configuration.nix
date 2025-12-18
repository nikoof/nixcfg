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
    ./style.nix
    ./containers/arch.nix
    ./containers/ubuntu.nix
  ];

  nix.settings.cores = 12;
  system.stateVersion = "24.05";
  nixpkgs.hostPlatform = "x86_64-linux";

  # -----------------[Boot]----------------------
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

  # -----------------[Networking]--------------------
  hardware.bluetooth.enable = true;
  networking = {
    hostName = "hofstadter";
    nameservers = [
      "9.9.9.9"
      "149.122.122.122"
    ];

    tempAddresses = "default";
    nftables.enable = false;
  };

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [];
    allowedUDPPorts = [];
  };

  services.resolved = {
    enable = true;
    dnsovertls = "opportunistic";
    extraConfig = ''
      MulticastDNS=yes
    '';
  };

  networking.networkmanager = {
    enable = true;
    wifi.macAddress = "random";
    ethernet.macAddress = "preserve";
  };
  # -----------------[Packages]--------------------
  environment.systemPackages = with pkgs; [
    inputs.agenix.packages."${system}".default

    man-pages
    man-pages-posix
    man-db

    eduvpn-client

    wineWow64Packages.full
    winetricks
  ];

  # ------------------[Users]------------------------
  users.users.nikoof = {
    description = "Nicolas Bratoveanu";
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager" "dialout" "tty" "plugdev" "uucd" "libvirtd" "optical" "cdrom" "ubridge" "adbusers" "kvm" "podman" "wireshark"];
  };

  home-manager = {
    extraSpecialArgs = {inherit inputs;};
    useGlobalPkgs = true;
    users.nikoof = ./users/nikoof.nix;
  };

  # -----------------[Desktop]------------------------
  desktop = {
    wm.xmonad.enable = true;
    xdgDirs.enable = true;
  };

  apps.util = {
    cli.enable = true;
    media.enable = true;
    monitoring.enable = true;
    networking.enable = true;
  };

  apps.gns3.enable = true;
  apps.gaming.steam.enable = true;
  apps.gaming.bottles.enable = true;

  # -----------------[Peripherals]---------------------
  peripherals.wacom.enable = true;
  peripherals.lexmark.enable = true;
  peripherals.nitrokey = {
    enable = true;
    enableSSHSupport = true;
  };

  # -----------------[Services]------------------------
  services.nixseparatedebuginfod2.enable = true;
  services.ratbagd.enable = true;

  # TODO: remove
  services.mysql = {
    enable = true;
    package = pkgs.mariadb;
  };

  programs.bash = {
    completion.enable = true;
    enableLsColors = true;
  };

  services.openssh.enable = true;
  services.chrony.enable = true;

  services.syncthing = {
    enable = true;
    user = "nikoof";
    dataDir = "/home/nikoof/Sync";
    configDir = "/home/nikoof/.config/syncthing";
  };

  services.ollama = {
    enable = false;
    acceleration = "cuda";
    package = pkgs.unstable.ollama-cuda;
  };

  services.gnome.gnome-keyring.enable = true;
  services.protonmail-bridge = {
    enable = true;
    path = with pkgs; [gnome-keyring];
  };

  services.udev = {
    # BBC micro:bit for Tock development
    extraRules = ''
      ACTION!="add|change", GOTO="openocd_rules_end"
      SUBSYSTEM!="usb|tty|hidraw", GOTO="openocd_rules_end"

      ATTRS{product}=="*CMSIS-DAP*", MODE="664", GROUP="plugdev"

      LABEL="openocd_rules_end"
    '';
  };

  # ------------------[Virtualisation]-------------------
  virtualisation.podman = {
    enable = true;
    autoPrune = {
      enable = true;
      dates = "monthly";
    };
  };

  programs.virt-manager.enable = true;
  virtualisation.spiceUSBRedirection.enable = true;
  virtualisation.libvirtd = {
    enable = true;
    qemu = {
      package = pkgs.qemu_kvm;
      runAsRoot = true;

      # Required for specialisations.dgpu-passthrough, but kept globally
      swtpm.enable = true;
    };
  };

  services.jupyterhub = {
    enable = false;
    jupyterlabEnv = pkgs.python3.withPackages (p:
      with p; [
        jupyterhub
        jupyterlab
        numpy
      ]);
  };
}
