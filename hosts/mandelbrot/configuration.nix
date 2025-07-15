{
  inputs,
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    "${inputs.nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
    ./hardware.nix
  ];

  nixpkgs.hostPlatform = "aarch64-linux";
  nixpkgs.buildPlatform = "x86_64-linux";

  sdImage.compressImage = false;
  sdImage.postBuildCommands = ''
    BL1=${pkgs.pkgsCross.aarch64-multiplatform.ubootOdroidC2}/bl1.bin.hardkernel
    UBOOT=${pkgs.pkgsCross.aarch64-multiplatform.ubootOdroidC2}/u-boot.gxbb
    dd if=$BL1 of=$img conv=notrunc bs=1 count=442
    dd if=$BL1 of=$img conv=notrunc bs=512 skip=1 seek=1
    dd if=$UBOOT of=$img conv=notrunc bs=512 seek=97
    sync
  '';

  services.openssh = {
    enable = true;
    settings.PermitRootLogin = "yes";
  };

  networking = {
    hostName = "mandelbrot";
    wireless = {
      enable = true;
      secretsFile = config.age.secrets.wireless.path;
    };
  };

  system.stateVersion = "25.05";
}
