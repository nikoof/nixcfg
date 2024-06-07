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
    nix.settings = {
      substituters = ["https://hyprland.cachix.org"];
      trusted-public-keys = ["hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="];
    };

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
