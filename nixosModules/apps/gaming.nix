{
  inputs,
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.apps.gaming;
in {
  options = {
    apps.gaming.bottles.enable = lib.mkEnableOption "Enable the Bottles Wine prefix manager";
    apps.gaming.steam.enable = lib.mkEnableOption "Enable Steam";
    apps.gaming.heroic.enable = lib.mkEnableOption "Enable Heroic Games Launcher";
    apps.gaming.lunar-client.enable = lib.mkEnableOption "Enable Lunar Minecraft Client";
  };

  config = {
    programs.steam = lib.mkIf cfg.steam.enable {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
    };

    environment.systemPackages = with pkgs;
      lib.lists.optionals cfg.heroic.enable [heroic]
      ++ lib.lists.optionals cfg.bottles.enable [bottles]
      ++ lib.lists.optionals cfg.lunar-client.enable [lunar-client];
  };
}
