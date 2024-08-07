{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    pre-commit.url = "github:cachix/pre-commit-hooks.nix";

    stylix.url = "github:danth/stylix";
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

    mkSystem = name: users: v: (nixpkgs.lib.nixosSystem {
      specialArgs = {inherit inputs pkgs;};
      modules =
        [
          self.outputs.nixosModules.default
          inputs.home-manager.nixosModules.home-manager
          inputs.stylix.nixosModules.stylix
          (v.path or ./hosts/${name}/configuration.nix)
        ]
        ++ map (user: ./users/${user}.nix) users;
    });
  in {
    nixosModules.default = ./nixos-modules;
    homeManagerModules.default = ./hm-modules;

    nixosConfigurations.gauss = mkSystem "gauss" ["nikoof"] {inherit pkgs;};
    nixosConfigurations.euler = mkSystem "euler" ["nikoof"] {inherit pkgs;};

    devShell.${system} = nixpkgs.legacyPackages.${system}.mkShell {
      inherit (self.checks.${system}.pre-commit-check) shellHook;

      packages = with pkgs; [lazygit];
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
