{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  systemd.targets.machines.enable = true;
  systemd.nspawn."arch" = {
    enable = true;
    execConfig = {
      Boot = true;
      ResolvConf = "bind-host";
    };

    networkConfig = {
      Private = false;
    };

    filesConfig = {
      BindReadOnly = [
        "/tmp/.X11-unix"
        "/dev/dri"
      ];
    };
  };

  systemd.services."systemd-nspawn@arch" = {
    enable = true;
    overrideStrategy = "asDropin";
    wantedBy = ["machines.target"];
  };
}
