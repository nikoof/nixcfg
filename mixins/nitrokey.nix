{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  hardware.nitrokey.enable = true;
  security.pam = {
    u2f = {
      enable = true;
      cue = true;
      authFile = "/etc/Nitrokey/u2f_keys";
    };

    services = {
      login.u2fAuth = true;
      sudo.u2fAuth = true;
      kde.u2fAuth = true;
      sddm.u2fAuth = true;
      polkit-1.u2fAuth = true;
    };
  };
}
