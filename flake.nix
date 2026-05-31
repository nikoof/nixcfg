{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs";

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
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

    wrappers = {
      url = "github:BirdeeHub/nix-wrapper-modules";
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
    system = "x86_64-linux";

    overlays = {
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

    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
      config.allowUnsupportedSystem = false;
      overlays = with overlays; [
        local
        unstable
        modifications
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

    # TODO: clean this up
    nvimModule = lib.modules.importApply ./nvim/module.nix inputs;
    nvimWrapper = wlib.evalModule nvimModule;
    nvim = nvimWrapper.config;
    nvimFull = nvimWrapper.config.apply {
      # See https://github.com/BirdeeHub/nix-wrapper-modules/discussions/506?discussioncomment-16818608
      specs.nix = {...}: {
        enable = true;
      };
      specs.c = {...}: {
        enable = true;
      };
      specs.rust = {...}: {
        enable = true;
      };
      specs.haskell = {...}: {
        enable = true;
      };
      specs.python = {...}: {
        enable = true;
      };
      specs.lua = {...}: {
        enable = true;
      };
      specs.java = {...}: {
        enable = true;
      };
      specs.typst = {...}: {
        enable = true;
      };
    };
  in {
    # Export packages as overlay to support cross-compilation.
    overlays.default = final: prev: import ../packages {pkgs = final;};

    # Packages exported here are built against nixpkgs-unstable.
    packages = eachSupportedSystem (
      system:
        (import ./packages) {
          pkgs = inputs.nixpkgs-unstable.legacyPackages.${system};
        }
        // {
          nvim = nvim.wrap {inherit pkgs;};
          nvimFull = nvimFull.wrap {inherit pkgs;};
        }
    );

    nixosModules.default = ./nixos-modules;
    homeManagerModules.default = ./hm-modules;

    nixosConfigurations.hofstadter = mkSystem "hofstadter" {};

    devShell.${system} = let
      xmonadGhc = pkgs.haskellPackages.ghcWithPackages (
        hp:
          with hp; [
            xmonad
            xmonad-contrib
            xmonad-extras
          ]
      );
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
