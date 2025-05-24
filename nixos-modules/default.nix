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
    nix.extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
    nix.settings = {
      substituters = [
        "https://nix-community.cachix.org"
        "https://hyprland.cachix.org"
        "https://cuda-maintainers.cachix.org"
        "https://ai.cachix.org"
      ];

      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
        "cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E="
        "ai.cachix.org-1:N9dzRK+alWwoKXQlnn0H6aUx0lU/mspIoz8hMvGvbbc="
      ];
    };

    environment.systemPackages = with pkgs; [
      nh
      nix-output-monitor
      nvd
      nixd
      deadnix
    ];

    programs.nix-ld.enable = true;

    environment.variables = rec {
      FLAKE = "/etc/nixos";
      NH_FLAKE = FLAKE;
    };
  };
}
