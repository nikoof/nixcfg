{
  config,
  inputs,
  pkgs,
  lib,
  ...
}: let
  cfg = config.desktop.hyprland;
in {
  options.desktop.hyprland = {
  };

  config = {
    programs.hyprland = {
      enable = true;
      package = inputs.hyprland.packages.${pkgs.system}.hyprland;
      xwayland.enable = true;
    };

    programs.hyprlock.enable = true;
    programs.hypridle.enable = true;
  };
}
