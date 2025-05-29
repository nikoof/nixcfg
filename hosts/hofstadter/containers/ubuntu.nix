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
      PrivateUsers = false;
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
        "/dev/video0"
        "/dev/video1"
        "/dev/media0"
        "/dev/v4l"
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
        "/dev/video0"
        "/dev/video1"
        "/dev/media0"
        "/dev/v4l"
        "block-loop rwm"
      ];
    };
  };
}
