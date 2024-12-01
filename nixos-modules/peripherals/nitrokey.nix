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

        python3Packages.pynitrokey
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
      };
    })
  ];
}
