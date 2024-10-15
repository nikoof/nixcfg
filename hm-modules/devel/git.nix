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
    programs.git = {
      enable = true;
      userName = "Nicolas Bratoveanu";
      userEmail = "nicolasbratoveanu@proton.me";

      lfs.enable = true;

      extraConfig = {
        safe = {directory = "/etc/nixos";};
        push = {autoSetupRemote = true;};
      };

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
