{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  users.users.nikoof = {
    description = "Nicolas Bratoveanu";
    isNormalUser = true;
    shell = "${pkgs.nushell}/bin/nu";
    extraGroups = ["wheel" "networkmanager" "dialout" "tty" "plugdev" "uucd" "libvirtd" "optical" "cdrom" "ubridge"];
  };

  home-manager = {
    extraSpecialArgs = {inherit inputs;};
    useGlobalPkgs = true;

    users.nikoof = ../home/nikoof.nix;
  };
}
