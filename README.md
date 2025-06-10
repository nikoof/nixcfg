# nixcfg
Personal Nix flake. Contains my nixos configurations, some nixos and home-manager modules and some obscure packages. Some slightly more notable ones:
* `boomer` - [tsoding](https://www.twitch.tv/tsoding)'s zooming in utility
* `dfm` - shell script dmenu-based file manager
* `sam` - a fork of [vidarh's sam fork](https://github.com/vidarh/SAM) with dynamic arrays
* `r2k` - romaji to kana input utility (I do not speak Japanese; still pretty cool software)

You can list out all the packages with `nix search github:nikoof/nixcfg . ^` (you can also use `nix search` to actually search).

## Layout
This has suffered several structural modifications throughout my experience learning Nix. Inspiration mostly from:
* [colemickens/nixcfg](https://github.com/colemickens/nixcfg)
* [vimjoyer](https://youtu.be/vYc6IzKvAJQ)

```
.
├── hm-modules       - home-manager modules
│   ├── apps
│   ├── devel
│   ├── shell
│   └── wm           - window manager configs
│       └── xmonad
├── hosts            - per-host nixos configurations
├── nixos-modules    - nixos modules; also contains some globally applicable nix-related utilities (lsp, nh, nom)
│   ├── apps
│   ├── desktop
│   └── peripherals
├── overlays         - nixpkgs overlays
├── packages         - home-grown packages
├── wallpapers       - desktop wallpapers
├── flake.lock
└── flake.nix
```

# Notes
The modules in this repo are not at all well-written, in that stuff that should be toggleable via options... isn't. This may or may not change at some point.
