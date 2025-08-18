{
  inputs,
  config,
  pkgs,
  lib,
  ...
}: {
  stylix = {
    enable = true;
    image = ../../wallpapers/moebius-bw.png;
    base16Scheme = {
      # Tomorrow Night Dark
      # by Chris Kempson (http://chriskempson.com)
      base00 = "000000"; # originally #1d1f21
      base01 = "282a2e";
      base02 = "373b41";
      base03 = "969896";
      base04 = "b4b7b4";
      base05 = "c5c8c6";
      base06 = "e0e0e0";
      base07 = "ffffff";
      base08 = "cc6666";
      base09 = "de935f";
      base0A = "f0c674";
      base0B = "b5bd68";
      base0C = "8abeb7";
      base0D = "81a2be";
      base0E = "b294bb";
      base0F = "a3685a";
    };

    opacity = {
      applications = 1.0;
      terminal = 0.8;
      desktop = 1.0;
      popups = 1.0;
    };

    fonts = {
      sizes = {
        applications = 10;
        terminal = 14;
        desktop = 10;
        popups = 10;
      };

      monospace = {
        package = pkgs.nerd-fonts.fira-code;
        name = "FiraCode Nerd Font Mono";
      };

      sansSerif = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Sans";
      };

      serif = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Serif";
      };
    };

    targets.plymouth.enable = false;
    targets.qt.enable = false;

    # INFO: these two targets add overlays to nixpkgs, which causes errors when using readOnlyPkgs.
    # However, due to how this is done in stylix, it is not possible to prevent it by simply disabling the targets.

    # targets.gnome-text-editor.enable = false;
    # targets.nixos-icons.enable = false;
  };
}
