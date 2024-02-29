{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [];

  networking.nftables.enable = true;
}
