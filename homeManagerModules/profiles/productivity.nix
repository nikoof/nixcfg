{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  cfg = config.profiles.productivity;
in {
  options = {
    profiles.productivity.enable = lib.mkEnableOption "Productivity apps";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      obsidian
      rnote
    ];
  };
}
