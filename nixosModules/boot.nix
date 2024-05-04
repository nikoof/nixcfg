{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  boot.loader = {
    systemd-boot.enable = true;
    systemd-boot.consoleMode = "max";
    efi.canTouchEfiVariables = true;
  };

  boot.plymouth = {
    enable = true;
    themePackages = with pkgs; [nixos-bgrt-plymouth];
    theme = "nixos-bgrt";
  };
}
