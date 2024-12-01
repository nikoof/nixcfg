{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot.initrd.availableKernelModules = ["xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-intel"];
  boot.extraModulePackages = [];
  hardware.firmware = [pkgs.linux-firmware];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/334da67a-d7cc-4da4-a6ad-3e3ab561cceb";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/965B-EF50";
    fsType = "vfat";
    options = ["fmask=0022" "dmask=0022"];
  };

  swapDevices = [{device = "/.swapfile";}];

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  hardware.enableRedistributableFirmware = true;
  hardware.cpu.intel.updateMicrocode = true;
  powerManagement.cpuFreqGovernor = "powersave";

  hardware.nvidia = {
    open = true;
    modesetting.enable = true;
    prime = {
      intelBusId = "PCI:0:2:0";
      nvidiaBusId = "PCI:1:0:0";
    };

    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      vaapiVdpau
      nvidia-vaapi-driver
    ];
  };
}
