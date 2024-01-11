{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  systemd.targets.machines.enable = true;
  systemd.nspawn."ubuntu-jammy" = {
    enable = true;
    execConfig = {
      Boot = true;
      ResolvConf = "bind-host";
    };
    networkConfig = {
      Private = false;
    };
  };
  systemd.services."systemd-nspawn@ubuntu-jammy" = {
    enable = true;
    overrideStrategy = "asDropin";
    wantedBy = ["machines.target"];
  };
}
