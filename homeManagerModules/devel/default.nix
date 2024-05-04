{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./git.nix
    ./languages.nix
  ];
}
