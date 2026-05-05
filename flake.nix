{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs";

    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    stylix.url = "github:danth/stylix/release-25.11";

    nh = {
      url = "github:nix-community/nh";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    lanzaboote = {
      url = "github:nix-community/lanzaboote";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixCats.url = "github:BirdeeHub/nixCats-nvim";

    cachyos-kernels.url = "github:xddxdd/nix-cachyos-kernel/release";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    ...
  }: let
    lib = nixpkgs.lib;
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

    supportedSystems = lib.systems.flakeExposed;
    eachSupportedSystem = lib.genAttrs supportedSystems;

    utils = inputs.nixCats.utils;
    nvim = import ./nvim {inherit inputs;};
    nixCatsBuilder =
      utils.baseBuilder nvim.luaPath {
        inherit pkgs;
      }
      nvim.categoryDefinitions
      nvim.packageDefinitions;
  in {
    overlays = overlays;
    nixosModules = ./nixos-modules;
    homeManagerModules.default = ./hm-modules;

    nixosConfigurations.hofstadter = mkSystem "hofstadter" {};

    packages = eachSupportedSystem (
      system:
        (import ./packages) {
          pkgs = inputs.nixpkgs-unstable.legacyPackages.${system};
        }
        // (builtins.listToAttrs (map (p: {
            name = p;
            value = nixCatsBuilder p;
          })
          nvim.packageNames))
    );

    devShell.${system} = let
      xmonadGhc = pkgs.haskellPackages.ghcWithPackages (hp:
        with hp; [
          xmonad
          xmonad-contrib
          xmonad-extras
        ]);
    in
      nixpkgs.legacyPackages.${system}.mkShell {
        packages = [
          inputs.agenix.packages.${system}.default

          pkgs.haskell-language-server
          xmonadGhc
        ];
      };

    formatter.${system} = pkgs.alejandra;
  };
}
