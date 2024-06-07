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

    ../../profiles/core.nix
    ../../profiles/gaming.nix

    ../../mixins/cups.nix
    ../../mixins/fonts.nix
    ../../mixins/pipewire.nix
    ../../mixins/plasma.nix

    ../../mixins/networking.nix
    ../../mixins/nitrokey.nix

    ./services/power.nix
    ./services/syncthing.nix

    ./containers/ubuntu-jammy.nix
  ];

  boot.loader = {
    systemd-boot.enable = true;
    systemd-boot.consoleMode = "max";
    efi.canTouchEfiVariables = true;
  };

  boot.plymouth = {
    enable = true;
    themePackages = with pkgs; [nixos-bgrt-plymouth];
    theme = "nixos-bgrt";
  };

  system.stateVersion = "23.05";
  nixpkgs.hostPlatform = "x86_64-linux";
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.initrd.availableKernelModules = ["xhci_pci" "thunderbolt" "vmd" "nvme" "usb_storage" "usbhid" "sd_mod"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-intel"];
  hardware.firmware = [pkgs.linux-firmware];

  hardware.bluetooth.enable = true;
  networking = {
    hostName = "euler";
    networkmanager.enable = true;
    firewall.enable = true;
    useDHCP = lib.mkDefault true;
  };

  swapDevices = [{device = "/swap/swapfile";}];
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/c10e2f83-8c02-4c75-99ab-40aaa3b71bb3";
      fsType = "btrfs";
      options = ["subvol=@"];
    };

    "/nix" = {
      device = "/dev/disk/by-uuid/c10e2f83-8c02-4c75-99ab-40aaa3b71bb3";
      fsType = "btrfs";
      options = ["subvol=@nix"];
    };

    "/home" = {
      device = "/dev/disk/by-uuid/c10e2f83-8c02-4c75-99ab-40aaa3b71bb3";
      fsType = "btrfs";
      options = ["subvol=@home"];
    };

    "/swap" = {
      device = "/dev/disk/by-uuid/c10e2f83-8c02-4c75-99ab-40aaa3b71bb3";
      fsType = "btrfs";
      options = ["subvol=@swap"];
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/E70B-B6CF";
      fsType = "vfat";
    };
  };

  hardware.enableRedistributableFirmware = true;
  hardware.cpu.intel.updateMicrocode = true;
  powerManagement.cpuFreqGovernor = "powersave";

  hardware.nvidia = {
    modesetting.enable = true;
    prime = {
      intelBusId = "PCI:0:2:0";
      nvidiaBusId = "PCI:1:0:0";
    };

    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;

    extraPackages = with pkgs; [
      vaapiVdpau
      nvidia-vaapi-driver
    ];
  };

  virtualisation.libvirtd.enable = true;
  environment.systemPackages = with pkgs; [
    qemu
    acpi
    lm_sensors
    wireguard-tools
    wl-clipboard
  ];

  services.udev = {
    # For Pi Pico debug probe (ipw)
    extraRules = ''
      SUBSYSTEM=="usb", ATTR{idVendor}=="2e8a", ATTR{idProduct}=="000c", MODE:="666"
    '';
  };
}
