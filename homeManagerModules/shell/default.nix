{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./bash.nix
    ./nushell.nix
    ./starship.nix
  ];
}
