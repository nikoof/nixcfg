{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./i3.nix
    ./dwm.nix
  ];
}
