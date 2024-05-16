{
  inputs,
  config,
  pkgs,
  lib,
  ...
}: {
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.initrd.availableKernelModules = ["xhci_pci" "thunderbolt" "vmd" "nvme" "usb_storage" "usbhid" "sd_mod"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-intel"];
  hardware.firmware = [pkgs.linux-firmware];

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
}
