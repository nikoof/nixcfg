{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs";
    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, home-manager, ... }:
    let
      system = "x86_64-linux";
      overlay-unstable = final: prev: {
        unstable = nixpkgs-unstable.legacyPackages.${prev.system};
      };
      pkgs = import nixpkgs {
	inherit system;
        config.allowUnfree = true;
	overlays = [ overlay-unstable ];
      };
    in {
    nixosConfigurations.nkbox = nixpkgs.lib.nixosSystem {
      inherit system;
      inherit pkgs;
      # specialArgs = { inherit pkgs; } // inputs;
      modules = [
	./hosts/nkbox-hw.nix
        ./hosts/nkbox.nix
	home-manager.nixosModules.home-manager {
	  home-manager = {
            useGlobalPkgs = true;
            users.nikoof = import ./users/nikoof/home.nix;
	  };
	}
      ];
    };

    nixosConfigurations.nkideapad = nixpkgs.lib.nixosSystem {
      inherit system;
      inherit pkgs;
      # specialArgs = { inherit pkgs; } // inputs;
      modules = [
	./hosts/nkideapad-hw.nix
        ./hosts/nkideapad.nix
	home-manager.nixosModules.home-manager {
	  home-manager = {
            useGlobalPkgs = true;
            users.nikoof = import ./users/nikoof/home.nix;
	  };
	}
      ];
    };
  };
}
