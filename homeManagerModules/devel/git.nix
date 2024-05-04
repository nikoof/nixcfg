{
  pkgs,
  inputs,
  config,
  lib,
  ...
}: {
  options = {
    devel.git.enable = lib.mkEnableOption "Enable Git";
    devel.git.signing = lib.mkEnableOption "Sign commits with Nitrokey";
    devel.git.github.enable = lib.mkEnableOption "Enable GitHub integration";
  };

  config = lib.mkIf config.devel.git.enable {
    programs.git = {
      enable = true;
      userName = "Nicolas Bratoveanu";
      userEmail = "nicolasbratoveanu@proton.me";

      extraConfig = {
        safe = {directory = "/etc/nixos";};
        push = {autoSetupRemote = true;};
      };

      signing = lib.mkIf config.devel.git.signing {
        signByDefault = true;
        key = "94B9F744D3E82C46";
      };
    };

    programs.gh = lib.mkIf config.devel.git.github.enable {
      enable = true;
      gitCredentialHelper.enable = true;
    };
  };
}
