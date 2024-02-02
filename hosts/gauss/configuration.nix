{
  inputs,
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    inputs.nixos-hardware.nixosModules.common-pc-ssd
    inputs.nixos-hardware.nixosModules.common-gpu-nvidia-nonprime

    ../../profiles/core.nix
    ../../profiles/gaming.nix

    ../../mixins/cups.nix
    ../../mixins/fonts.nix
    ../../mixins/pipewire.nix
    ../../mixins/plasma.nix
    ../../mixins/gns3.nix

    ./services/syncthing.nix
  ];

  system.stateVersion = "23.05";
  nixpkgs.hostPlatform = "x86_64-linux";

  networking = {
    hostName = "gauss";
    networkmanager.enable = true;
    firewall.enable = true;
    tempAddresses = "disabled";
    interfaces.enp3s0.wakeOnLan.enable = true;
    interfaces.enp3s0.useDHCP = true;
  };

  services.xserver.xrandrHeads = [
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

  boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" "sr_mod"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-intel"];
  boot.extraModulePackages = [];

  swapDevices = [{device = "/swap/swapfile";}];
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/58f4e5f7-20ca-41ba-a073-366ee94fdf3a";
      fsType = "btrfs";
      options = ["subvol=@" "compress=zstd:3" "space_cache=v2" "ssd"];
    };

    "/nix" = {
      device = "/dev/disk/by-uuid/58f4e5f7-20ca-41ba-a073-366ee94fdf3a";
      fsType = "btrfs";
      options = ["subvol=@nix" "compress=zstd:3" "space_cache=v2" "ssd" "noatime"];
    };

    "/home" = {
      device = "/dev/disk/by-uuid/00416392-9379-4110-b74d-e9f04dda1e0b";
      fsType = "btrfs";
      options = ["subvol=@home" "compress=zstd:3" "space_cache=v2"];
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/D598-27BA";
      fsType = "vfat";
    };

    "/swap" = {
      device = "/dev/disk/by-uuid/58f4e5f7-20ca-41ba-a073-366ee94fdf3a";
      fsType = "btrfs";
      options = ["subvol=@swap" "compress=zstd:3" "space_cache=v2" "ssd" "noatime"];
    };
  };

  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  powerManagement.cpuFreqGovernor = "performance";

  hardware.nvidia = {
    modesetting.enable = true;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;

    extraPackages = with pkgs; [
      nvidia-vaapi-driver
    ];
  };
}
