{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./devel
    ./terminal
    ./profiles
    ./apps
  ];
}
