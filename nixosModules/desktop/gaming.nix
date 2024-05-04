{
  inputs,
  config,
  lib,
  pkgs,
  ...
}: {
  options = {
    desktop.gaming.enable = lib.mkEnableOption "Enable gaming apps";
    desktop.gaming.steam.enable = lib.mkEnableOption "Enable Steam";
    desktop.gaming.heroic.enable = lib.mkEnableOption "Enable Heroic Games Launcher";
    desktop.gaming.victoria2Server.openFirewall = lib.mkEnableOption "Open firewall for Victoria 2 server";
  };

  config = lib.mkIf config.desktop.gaming.enable {
    programs.gamemode.enable = true;
    programs.steam = lib.mkIf config.desktop.gaming.steam.enable {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
    };

    environment.systemPackages = with pkgs;
      lib.mkIf config.desktop.gaming.heroic.enable [
        heroic
      ];

    networking.firewall.allowedTCPPortRanges = lib.mkIf config.desktop.gaming.victoria2Server.openFirewall [
      {
        from = 1630;
        to = 1641;
      }
      {
        from = 1714;
        to = 1764;
      }
    ];
    networking.firewall.allowedUDPPortRanges = lib.mkIf config.desktop.gaming.victoria2Server.openFirewall [
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
