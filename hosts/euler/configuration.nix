{
  inputs,
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    inputs.nixos-hardware.nixosModules.common-pc-laptop
    inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
    inputs.nixos-hardware.nixosModules.common-gpu-nvidia

    ./hardware.nix

    ./services/syncthing.nix

    ./containers/ubuntu-jammy.nix
  ];

  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/nord.yaml";
    image = ../../wallpapers/moebius.png;
  };

  stylix.cursor.name = "Breeze";

  stylix.opacity = {
    applications = 1.0;
    terminal = 0.9;
    desktop = 1.0;
    popups = 1.0;
  };

  stylix.fonts = {
    sizes = {
      applications = 12;
      terminal = 15;
      desktop = 10;
      popups = 10;
    };

    monospace = {
      package = pkgs.nerdfonts.override {fonts = ["FiraCode"];};
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

  boot.loader = {
    systemd-boot.enable = true;
    systemd-boot.consoleMode = "max";
    efi.canTouchEfiVariables = true;
  };

  system.stateVersion = "23.05";
  nixpkgs.hostPlatform = "x86_64-linux";

  hardware.bluetooth.enable = true;
  networking = {
    hostName = "euler";
    networkmanager.enable = true;
    firewall.enable = true;
    useDHCP = lib.mkDefault true;
  };

  virtualisation.libvirtd.enable = true;
  virtualisation.spiceUSBRedirection.enable = true;

  environment.systemPackages = with pkgs; [
    qemu
    acpi
    lm_sensors
    wireguard-tools
    wl-clipboard
  ];

  desktop = {
    xdgDirs.enable = true;
    plasma.enable = true;

    printing.enable = true;
    printing.autodetect = true;
  };

  apps.gns3.enable = true;

  apps.gaming = {
    bottles.enable = true;
    steam.enable = true;
    mangohud.enable = true;

    victoria2Server.openFirewall = true;
  };

  peripherals.wacom.enable = true;
  peripherals.nitrokey = {
    enable = true;
    enableSSHSupport = true;
  };

  services.openssh.enable = true;
  services.ollama = {
    enable = true;
    acceleration = "cuda";
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
  services.udev = {
    # For Pi Pico debug probe (ipw)
    extraRules = ''
      SUBSYSTEM=="usb", ATTR{idVendor}=="2e8a", ATTR{idProduct}=="000c", MODE:="666"
    '';
  };

  programs.dconf.enable = true;
}
