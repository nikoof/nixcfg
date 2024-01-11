{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [];

  networking.firewall.allowedTCPPortRanges = [
    {
      from = 1630;
      to = 1641;
    }
    {
      from = 1714;
      to = 1764;
    }
  ];
  networking.firewall.allowedUDPPortRanges = [
    {
      from = 1630;
      to = 1641;
    }
    {
      from = 1714;
      to = 1764;
    }
  ];
}
