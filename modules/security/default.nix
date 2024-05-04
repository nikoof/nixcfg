{
  config,
  inputs,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./nitrokey.nix
  ];
}
