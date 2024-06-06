{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./boot.nix
    ./variables.nix

    ./apps
    ./desktop
    ./hardware
    ./security
    ./wm
  ];

  config = {
    desktop.enable = lib.mkDefault true;
    desktop.pipewire.enable = lib.mkDefault true;
    desktop.redshift.enable = lib.mkDefault true;
    desktop.printing.enable = lib.mkDefault true;
    desktop.printing.autodetect = lib.mkDefault true;

    nix.settings.experimental-features = ["nix-command" "flakes"];
    environment.systemPackages = with pkgs; [
      unstable.nh
      nix-output-monitor
      nvd
    ];
  };
}
