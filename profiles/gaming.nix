{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./core.nix

    ../mixins/steam.nix
    ../mixins/firewall-vic2.nix
  ];
}
