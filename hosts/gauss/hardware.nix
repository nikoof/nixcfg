{
  inputs,
  config,
  pkgs,
  lib,
  ...
}: {
  boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" "sr_mod"];
  boot.initrd.kernelModules = [];
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelModules = ["kvm-intel"];
  boot.extraModulePackages = [];

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

  swapDevices = [{device = "/swap/swapfile";}];
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/7e1430a9-e59e-430a-a367-e2b823588684";
      fsType = "btrfs";
      options = ["subvol=@" "compress=zstd:3" "space_cache=v2" "ssd"];
    };

    "/nix" = {
      device = "/dev/disk/by-uuid/7e1430a9-e59e-430a-a367-e2b823588684";
      fsType = "btrfs";
      options = ["subvol=@nix" "compress=zstd:3" "space_cache=v2" "ssd" "noatime"];
    };

    "/home" = {
      device = "/dev/disk/by-uuid/00416392-9379-4110-b74d-e9f04dda1e0b";
      fsType = "btrfs";
      options = ["subvol=@home" "compress=zstd:3" "space_cache=v2"];
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/2393-1A7C";
      fsType = "vfat";
    };

    "/swap" = {
      device = "/dev/disk/by-uuid/7e1430a9-e59e-430a-a367-e2b823588684";
      fsType = "btrfs";
      options = ["subvol=@swap" "compress=zstd:3" "space_cache=v2" "ssd" "noatime"];
    };
  };

  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  powerManagement.cpuFreqGovernor = "performance";
}
