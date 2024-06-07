{
  config,
  inputs,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./gaming.nix
    ./gns3.nix
    ./libreoffice.nix
  ];
}
