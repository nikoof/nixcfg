{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pre-commit = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    stylix.url = "github:danth/stylix/release-25.11";

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    lanzaboote = {
      url = "github:nix-community/lanzaboote";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    nixCats.url = "github:BirdeeHub/nixCats-nvim";
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
      config.allowUnsupportedSystem = false;
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

    utils = inputs.nixCats.utils;
    nvim = import ./nvim {inherit inputs;};
    nixCatsBuilder =
      utils.baseBuilder nvim.luaPath {
        inherit pkgs;
      }
      nvim.categoryDefinitions
      nvim.packageDefinitions;
  in
    {
      nixosModules.default = ./nixos-modules;
      homeManagerModules.default = ./hm-modules;

      nixosConfigurations.hofstadter = mkSystem "hofstadter" {};

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
        thesePkgs =
          (import ./packages) {
            pkgs = nixpkgs.legacyPackages.${system};
          }
          // (builtins.listToAttrs (map (p: {
              name = p;
              value = nixCatsBuilder p;
            })
            nvim.packageNames));
      in {
        packages = thesePkgs;
      }
    );
}
