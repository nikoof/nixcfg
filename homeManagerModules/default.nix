{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./apps
    ./devel
    ./shell
  ];
}
