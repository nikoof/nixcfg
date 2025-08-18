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
    enable = lib.mkEnableOption "Enable Nitrokey support.";
    enableSSHSupport = lib.mkEnableOption "Enable using Nitrokey for SSH authentication.";
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      hardware.nitrokey.enable = true;
      # services.pcscd.enable = true;
      services.dbus.packages = [pkgs.gcr];

      environment.systemPackages = with pkgs; [
        python3Packages.pynitrokey
        pinentry
        pinentry-gtk2
      ];

      security.pam = {
        u2f = {
          enable = true;
          settings = {
            cue = true;
            control = "sufficient";
            authFile = "/etc/Nitrokey/u2f_keys";
          };
        };

        services = {
          login.u2fAuth = true;
          sudo.u2fAuth = true;
        };
      };
    })
    (lib.mkIf cfg.enableSSHSupport {
      programs.mtr.enable = true;
      programs.gnupg.agent = {
        enable = true;
        enableSSHSupport = true;
        pinentryPackage = pkgs.pinentry-gtk2;
      };
    })
  ];
}
