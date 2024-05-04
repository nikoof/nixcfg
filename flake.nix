{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/release-23.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixvim = {
      url = "github:nix-community/nixvim/nixos-23.11";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    pre-commit.url = "github:cachix/pre-commit-hooks.nix";

    pipewire-screenaudio = {
      url = "github:IceDBorn/pipewire-screenaudio";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
      config.permittedInsecurePackages = [
        "electron-25.9.0"
      ];
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
          (v.path or ./hosts/${name}/configuration.nix)
        ]
        ++ map (user: ./users/${user}.nix) users;
    });
  in {
    nixosModules.default = ./nixosModules;
    homeManagerModules.default = ./homeManagerModules;

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
