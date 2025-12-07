{
  pkgs,
  inputs,
  config,
  lib,
  ...
}: let
  cfg = config.devel.git;
in {
  options.devel.git = {
    enable = lib.mkEnableOption "Enable Git";
    signing = lib.mkEnableOption "Sign commits with Nitrokey";
    github.enable = lib.mkEnableOption "Enable GitHub integration";
    lazygit.enable = lib.mkEnableOption "Enable lazygit frontend";
  };

  config = lib.mkIf cfg.enable {
    programs.delta = {
      enable = true;
      enableGitIntegration = true;
    };

    programs.git = {
      enable = true;
      settings = {
        user.name = "Nicolas Bratoveanu";
        user.email = "nicolasbratoveanu@proton.me";
        safe = {directory = "/etc/nixos";};
        push = {autoSetupRemote = true;};
      };

      lfs.enable = true;

      signing = lib.mkIf cfg.signing {
        signByDefault = true;
        key = "94B9F744D3E82C46";
      };
    };

    programs.gh = lib.mkIf cfg.github.enable {
      enable = true;
      gitCredentialHelper.enable = true;
    };

    programs.lazygit = lib.mkIf cfg.lazygit.enable {
      enable = true;
    };
  };
}
