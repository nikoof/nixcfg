{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  cfg = config.peripherals.nitrokey;
in {
  options.peripherals.nitrokey = {
    enable = lib.mkEnableOption "Enable nitrokey support";
    enableSSHSupport = lib.mkEnableOption "Enable support for using Nitrokey for SSH authentication";
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      hardware.nitrokey.enable = true;

      # TODO: Install based on DE
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
    })
    (lib.mkIf cfg.enableSSHSupport {
      programs.mtr.enable = true;
      programs.gnupg.agent = {
        enable = true;
        enableSSHSupport = true;
      };
    })
  ];
}
