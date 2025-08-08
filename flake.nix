{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    pre-commit.url = "github:cachix/pre-commit-hooks.nix";
    stylix.url = "github:danth/stylix/release-25.05";

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    lanzaboote = {
      url = "github:nix-community/lanzaboote";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nh = {
      url = "github:nix-community/nh";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    flake-utils,
    pre-commit,
    ...
  }: let
    system = "x86_64-linux";
    overlays = import ./overlays {inherit inputs;};
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
      config.allowUnsupportedSystem = true;
      overlays = with overlays; [
        local-packages
        modifications
        unstable-packages
      ];
    };

    pkgsArm = import nixpkgs {
      system = "aarch64-linux";
      config.allowUnfree = true;
      config.allowUnsupportedSystem = true;
      overlays = with overlays; [
        local-packages
        modifications
        unstable-packages
      ];
    };

    mkSystem = name: v: (nixpkgs.lib.nixosSystem {
      specialArgs = {inherit inputs;};
      modules = [
        # INFO: This does not currently work with stylix, as it adds overlays to nixpkgs.
        # inputs.nixpkgs.nixosModules.readOnlyPkgs

        {nixpkgs.pkgs = pkgs;}
        self.outputs.nixosModules.default
        inputs.home-manager.nixosModules.home-manager
        inputs.stylix.nixosModules.stylix
        inputs.agenix.nixosModules.default
        ./secrets
        (v.path or ./hosts/${name}/configuration.nix)
      ];
    });
  in
    {
      nixosModules.default = ./nixos-modules;
      homeManagerModules.default = ./hm-modules;

      nixosConfigurations.hofstadter = mkSystem "hofstadter" {};
      nixosConfigurations.mandelbrot = nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs;};
        modules = [
          inputs.agenix.nixosModules.default
          ./secrets
          ./hosts/mandelbrot/configuration.nix
        ];
      };

      images = {
        mandelbrot = self.nixosConfigurations.mandelbrot.config.system.build.sdImage;
      };

      devShell.${system} = let
        xmonadGhc = pkgs.haskellPackages.ghcWithPackages (hp:
          with hp; [
            xmonad
            xmonad-contrib
            xmonad-extras
          ]);
      in
        nixpkgs.legacyPackages.${system}.mkShell {
          inherit (self.checks.${system}.pre-commit-check) shellHook;

          packages = [
            inputs.agenix.packages.${system}.default

            pkgs.haskell-language-server
            xmonadGhc
          ];
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
    }
    // flake-utils.lib.eachDefaultSystem (
      system: let
        thesePkgs = (import ./packages) {
          pkgs = nixpkgs.legacyPackages.${system};
        };
      in {
        packages = thesePkgs;
      }
    );
}
