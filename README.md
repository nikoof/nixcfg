# nixcfg
This has suffered several structural modifications throughout my experience learning Nix. Inspiration mostly from:
* [colemickens/nixcfg](https://github.com/colemickens/nixcfg)
* [vimjoyer](https://youtu.be/vYc6IzKvAJQ)

## Layout
* `home`
    * top-level home-manager modules (per-user)
* `homeManagerModules`
    * modularised home-manager configurations for apps, terminal and shells
    * `profiles`
        * not the most ideally named module, but it contains groups of packages that I pretty much use as bundles
* `hosts`
    * top-level NixOS modules (per-host)
* `nixosModules`
    * modularised NixOS configurations for apps, services and desktops
    * contains some ~~less modular~~ common configuration
* `overlays`
    * contains overlays for nixpkgs:
        * local - packages not in nixpkgs, declared locally in `packages`
        * unstable - the unstable branch of nixpkgs
        * modifications - overrides of nixpkgs packages
* `packages`
    * local packages
* `users`
    * user account declarations

