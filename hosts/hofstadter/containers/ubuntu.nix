{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  systemd.targets.machines.enable = true;
  systemd.nspawn."ubuntu" = {
    enable = true;
    execConfig = {
      Boot = true;
      ResolvConf = "bind-host";
    };

    networkConfig = {
      Private = false;
    };

    filesConfig = {
      Bind = [
        "/dev/dri"
        "/dev/nvidia0"
        "/dev/nvidiactl"
        "/dev/nvidia-modeset"
        "/dev/nvidia-uvm"
        "/dev/nvidia-uvm-tools"
        "/dev/input"
        "/dev/shm"
      ];

      BindReadOnly = [
        "/tmp/.X11-unix"
        "/run/user/1000/pulse:/run/user/host/pulse/"
      ];
    };
  };

  systemd.services."systemd-nspawn@ubuntu" = {
    enable = true;
    overrideStrategy = "asDropin";
    wantedBy = ["machines.target"];
    serviceConfig = {
      DeviceAllow = [
        "/dev/nvidiactl"
        "/dev/nvidia0"
        "/dev/nvidia-modeset"
        "/dev/nvidia-uvm"
        "/dev/nvidia-uvm-tools"
        "block-loop rwm"
      ];
    };
  };
}
