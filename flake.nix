{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, nixos-hardware, home-manager, ... }:
    let
      system = "x86_64-linux";
      overlay-unstable = final: prev: {
        unstable = import nixpkgs-unstable {
          inherit system;
          config.allowUnfree = true;
	};
      };
      pkgs = import nixpkgs {
	inherit system;
        config.allowUnfree = true;
	overlays = [ overlay-unstable ];
      };
      localPkgs = import ./packages { inherit pkgs; };
    in {
    nixosConfigurations.nkbox = nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = { inherit pkgs localPkgs; };
      modules = [
	./hardware/nkbox.nix
	./hosts/common.nix
        ./hosts/nkbox.nix
	home-manager.nixosModules.home-manager {
	  home-manager = {
            useGlobalPkgs = true;
            users.nikoof = import ./users/nikoof;
	  };
	}
      ];
    };

    nixosConfigurations.nkideapad = nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = { inherit pkgs localPkgs; };
      modules = [
        nixos-hardware.nixosModules.common-pc-laptop-hdd
	nixos-hardware.nixosModules.common-gpu-intel
	./hardware/nkideapad.nix
	./hosts/common.nix
        ./hosts/nkideapad.nix
	home-manager.nixosModules.home-manager {
	  home-manager = {
            useGlobalPkgs = true;
            users.nikoof = import ./users/nikoof;
	  };
	}
      ];
    };
  };
}
