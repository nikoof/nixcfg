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
      ];

      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };

    environment.systemPackages = with pkgs; [
      nix-output-monitor
      nvd
      nixd
      deadnix
    ];

    programs.nix-ld.enable = true;
    programs.nh = {
      enable = true;
      clean.enable = true;
      clean.extraArgs = "--nogcroots";
      flake = "/etc/nixos";
    };

    environment.variables = {
      FLAKE = "/etc/nixos";
    };
  };
}
