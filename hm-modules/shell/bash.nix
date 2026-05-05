{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.shell.bash;
in {
  options.shell.bash = {
    enable = lib.mkEnableOption "Enable bash";
  };

  config = lib.mkIf cfg.enable {
    programs.bash = {
      enable = true;
    };
  };
}
