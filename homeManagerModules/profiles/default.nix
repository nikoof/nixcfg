{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./entertainment.nix
    ./school.nix
    ./media.nix
    ./productivity.nix
  ];
}
