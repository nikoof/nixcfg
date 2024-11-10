{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.shell.nushell;
in {
  options.shell.nushell = {
    enable = lib.mkEnableOption "Enable nushell";
  };

  config = lib.mkIf cfg.enable {
    programs.nushell = {
      enable = true;
      configFile.text = ''
        $env.config = {
          show_banner: false,
        }
      '';
    };
  };
}
