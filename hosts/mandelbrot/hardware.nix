{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: {
  imports = [];

  boot.initrd.availableKernelModules = ["xhci_pci" "usb_storage" "sd_mod"];
  boot.kernelModules = [];
  boot.extraModulePackages = [];

  hardware.firmware = [pkgs.linux-firmware];
  hardware.enableRedistributableFirmware = true;

  powerManagement.cpuFreqGovernor = "performance";
}
