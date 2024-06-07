{
  inputs,
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.apps.gaming;
in {
  options.apps.gaming = {
    bottles.enable = lib.mkEnableOption "Enable the Bottles Wine prefix manager";
    steam.enable = lib.mkEnableOption "Enable Steam";
    heroic.enable = lib.mkEnableOption "Enable Heroic Games Launcher";
    lunar-client.enable = lib.mkEnableOption "Enable Lunar Minecraft Client";
    mangohud.enable = lib.mkEnableOption "Enable Mangohud";

    victoria2Server.openFirewall = lib.mkEnableOption "Open firewall for Victoria 2 server";
  };

  config = {
    programs.gamemode.enable = lib.mkDefault cfg.steam.enable;
    programs.steam = lib.mkIf cfg.steam.enable {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
    };

    environment.systemPackages = with pkgs;
      lib.lists.optionals cfg.mangohud.enable [mangohud]
      ++ lib.lists.optionals cfg.heroic.enable [heroic]
      ++ lib.lists.optionals cfg.bottles.enable [bottles]
      ++ lib.lists.optionals cfg.lunar-client.enable [lunar-client];

    networking.firewall.allowedTCPPortRanges = lib.mkIf cfg.victoria2Server.openFirewall [
      {
        from = 1630;
        to = 1641;
      }
      {
        from = 1714;
        to = 1764;
      }
    ];
    networking.firewall.allowedUDPPortRanges = lib.mkIf cfg.victoria2Server.openFirewall [
      {
        from = 1630;
        to = 1641;
      }
      {
        from = 1714;
        to = 1764;
      }
    ];
  };
}
