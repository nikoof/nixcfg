{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ../users/nikoof
    inputs.home-manager.nixosModules.home-manager
  ];

  users.users.nikoof = {
    description = "Nicolas Bratoveanu";
    isNormalUser = true;
    shell = "${pkgs.nushell}/bin/nu";
    extraGroups = ["wheel" "networkmanager" "dialout" "tty" "plugdev" "uucd" "libvirtd" "optical" "cdrom"];
  };
}
