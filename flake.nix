{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixvim = {
      url = "github:nix-community/nixvim/nixos-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pre-commit.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    nixos-hardware,
    flake-utils,
    pre-commit,
    ...
  }: let
    system = "x86_64-linux";
    overlays = import ./overlays {inherit inputs;};
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = with overlays; [
        local-packages
        modifications
        unstable-packages
      ];
    };
  in rec {
    nixosConfigurations.gauss = nixpkgs.lib.nixosSystem {
      specialArgs = {inherit system inputs pkgs;};
      modules = with nixos-hardware.nixosModules; [
        common-pc-ssd
        common-gpu-nvidia-nonprime
        ./hosts/gauss/hardware.nix
        ./hosts/gauss/configuration.nix
      ];
    };

    nixosConfigurations.euler = nixpkgs.lib.nixosSystem {
      specialArgs = {inherit system inputs pkgs;};
      modules = with nixos-hardware.nixosModules; [
        common-pc-laptop
        common-pc-laptop-ssd
        common-gpu-nvidia
        ./hosts/euler/hardware.nix
        ./hosts/euler/configuration.nix
      ];
    };

    devShell.${system} = nixpkgs.legacyPackages.${system}.mkShell {
      inherit (self.checks.${system}.pre-commit-check) shellHook;
    };

    checks.${system} = {
      pre-commit-check = pre-commit.lib.${system}.run {
        src = ./.;
        hooks = {
          alejandra.enable = true;
        };
      };
    };

    formatter.${system} = pkgs.alejandra;
  };
}
