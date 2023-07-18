{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
	inherit system;
        config.allowUnfree = true;
	overlays = [];
      };
    in {
    nixosConfigurations.nkbox = nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = { inherit pkgs; } // inputs;
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
  };
}
