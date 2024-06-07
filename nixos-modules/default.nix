{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./apps
    ./desktop
    ./peripherals
  ];

  config = {
    nix.settings.experimental-features = ["nix-command" "flakes"];
    environment.systemPackages = with pkgs; [
      nh
      nix-output-monitor
      nvd
    ];

    environment.variables = {
      FLAKE = "/etc/nixos";
    };
  };
}
