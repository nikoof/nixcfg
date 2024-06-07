{
  pkgs,
  lib,
  inputs,
  config,
  ...
}: {
  imports = [
    ./hyprland.nix
  ];
}
