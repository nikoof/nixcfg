{
  config,
  lib,
  pkgs,
  modulesPath,
  inputs,
  ...
}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    inputs.nixos-hardware.nixosModules.common-gpu-intel
  ];

  boot.initrd.availableKernelModules = ["xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod"];
  specialisation = {
    passthrough-dgpu = {
      configuration = {
        boot.initrd.kernelModules = [
          "vfio_pci"
          "vfio"
          "vfio_iommu_type1"
        ];
        boot.kernelParams = [
          "intel_iommu=on"
          "vfio-pci.ids=10de:25bc"
        ];
      };
    };
  };
  boot.kernelModules = ["kvm-intel"];
  boot.extraModulePackages = [];
  hardware.firmware = [pkgs.linux-firmware];

  swapDevices = [{device = "/swap/swapfile";}];
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/6f441c28-49bc-41ea-a9ce-f35af86cc43a";
      fsType = "btrfs";
      options = ["subvol=@" "compress=zstd:3" "space_cache=v2" "ssd"];
    };

    "/nix" = {
      device = "/dev/disk/by-uuid/6f441c28-49bc-41ea-a9ce-f35af86cc43a";
      fsType = "btrfs";
      options = ["subvol=@nix" "compress=zstd:3" "space_cache=v2" "ssd" "noatime"];
    };

    "/home" = {
      device = "/dev/disk/by-uuid/6f441c28-49bc-41ea-a9ce-f35af86cc43a";
      fsType = "btrfs";
      options = ["subvol=@home" "compress=zstd:3" "space_cache=v2" "ssd"];
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/965B-EF50";
      fsType = "vfat";
      options = ["fmask=0022" "dmask=0022"];
    };

    "/swap" = {
      device = "/dev/disk/by-uuid/6f441c28-49bc-41ea-a9ce-f35af86cc43a";
      fsType = "btrfs";
      options = ["subvol=@swap" "compress=zstd:3" "space_cache=v2" "ssd" "noatime"];
    };
  };

  # fileSystems."/" = {
  #   device = "/dev/disk/by-uuid/334da67a-d7cc-4da4-a6ad-3e3ab561cceb";
  #   fsType = "ext4";
  # };

  # fileSystems."/boot" = {
  #   device = "/dev/disk/by-uuid/965B-EF50";
  #   fsType = "vfat";
  #   options = ["fmask=0022" "dmask=0022"];
  # };

  # swapDevices = [{device = "/.swapfile";}];

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

  hardware.intelgpu = {
    vaapiDriver = "intel-media-driver";
    enableHybridCodec = true;
  };

  environment.sessionVariables = {LIBVA_DRIVER_NAME = "iHD";};
  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      intel-vaapi-driver
      intel-media-driver
      vpl-gpu-rt
      nvidia-vaapi-driver
      vaapiVdpau
    ];
  };

  services.thermald.enable = true;
}
