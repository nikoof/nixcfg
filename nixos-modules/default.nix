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
      substituters = [
        "https://nix-community.cachix.org"
        "https://hyprland.cachix.org"
        "https://cuda-maintainers.cachix.org"
      ];

      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
        "cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E="
      ];
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
