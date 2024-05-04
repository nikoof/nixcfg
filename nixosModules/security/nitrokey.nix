{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  options = {
    security.nitrokey.enable = lib.mkEnableOption "Enable nitrokey support";
    security.nitrokey.enableSSHSupport = lib.mkEnableOption "Enable support for using Nitrokey for SSH authentication";
  };

  config =
    lib.mkIf config.security.nitrokey.enable {
      hardware.nitrokey.enable = true;

      environment.systemPackages = with pkgs; [
        pinentry
        pinentry-qt
      ];
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
    // lib.mkIf config.security.nitrokey.enableSSHSupport {
      programs.mtr.enable = true;
      programs.gnupg.agent = {
        enable = true;
        enableSSHSupport = true;
      };
    };
}
