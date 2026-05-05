{
  config,
  inputs,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./nitrokey.nix
    ./wacom.nix
    ./lexmark.nix
  ];
}
