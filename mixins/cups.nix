{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  services.printing.enable = true;
  services.avahi = {
    enable = true;
    nssmdns = true;
    openFirewall = true;
  };
}
