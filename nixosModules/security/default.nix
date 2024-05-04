{
  config,
  inputs,
  pkgs,
  lib,
  ...
}: let
  cfg = config.security;
in {
  imports = [
    ./nitrokey.nix
  ];

  options = {
    security.keepassxc.enable = lib.mkEnableOption "Enable KeePassXC";
  };

  config = {
    environment.systemPackages = with pkgs;
      lib.lists.optionals cfg.keepassxc.enable [
        keepassxc
      ];
  };
}
