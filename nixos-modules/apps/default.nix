{
  config,
  inputs,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./gaming.nix
    ./util.nix
    ./gns3.nix
  ];
}
