{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs";

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    stylix.url = "github:danth/stylix/release-26.05";

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

    wrappers = {
      url = "github:nix-community/nix-wrapper-modules";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cachyos-kernels.url = "github:xddxdd/nix-cachyos-kernel/release";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    nixpkgs-unstable,
    wrappers,
    ...
  }: let
    lib = nixpkgs.lib;
    wlib = wrappers.lib;

    supportedSystems = lib.systems.flakeExposed;
    eachSupportedSystem = lib.genAttrs supportedSystems;

    mkSystem = name: system: opt: (nixpkgs.lib.nixosSystem {
      specialArgs = {inherit inputs;};
      modules = [
        inputs.nixpkgs.nixosModules.readOnlyPkgs
        {
          nixpkgs.pkgs = import nixpkgs {
            inherit system;

            config = {
              allowUnfree = true;
              allowUnsupportedSystem = false;
            };

            overlays = builtins.attrValues {
              modifications = import ./overlays/modifications.nix;

              local = final: prev: {
                local = import ./packages {pkgs = final;};
              };

              unstable = final: prev: {
                unstable = import nixpkgs-unstable {
                  inherit (final) config;
                  inherit (final.stdenv.hostPlatform) system;
                };
              };
            };
          };
        }
        self.outputs.nixosModules.default
        (opt.path or ./hosts/${name}/configuration.nix)
      ];
    });
  in {
    nixosModules.default = ./nixos-modules;
    homeManagerModules.default = ./hm-modules;
    wrapperModules = import ./wrapper-modules {inherit inputs lib;};
    wrappers = builtins.mapAttrs (_: value: (wlib.evalModule value).config) self.wrapperModules;

    nixosConfigurations.hofstadter = mkSystem "hofstadter" "x86_64-linux" {};

    # Export packages as overlay to support cross-compilation.
    overlays.default = final: prev: import ./packages {pkgs = final;};

    # Packages exported here are built against nixpkgs-unstable.
    packages = eachSupportedSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in
        (import ./packages) {
          pkgs = inputs.nixpkgs-unstable.legacyPackages.${system};
        }
        // builtins.mapAttrs (_: value: value.config.wrap {inherit pkgs;}) self.wrapperModules
        // {
          # I can't think of a nicer way to get this one in here other than having the wrapper modules also specify their packages...
          nvimFull = (self.wrappers.nvim.apply {full = true;}).wrap {inherit pkgs;};
        }
    );

    devShells = eachSupportedSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
        xmonadGhc = pkgs.haskellPackages.ghcWithPackages (
          hp:
            with hp; [
              xmonad
              xmonad-contrib
              xmonad-extras
            ]
        );
      in {
        default = nixpkgs.legacyPackages.${system}.mkShell {
          packages = [
            inputs.agenix.packages.${system}.default

            pkgs.haskell-language-server
            xmonadGhc
          ];
        };
      }
    );

    formatter = eachSupportedSystem (system: nixpkgs.legacyPackages.${system}.alejandra);
  };
}
