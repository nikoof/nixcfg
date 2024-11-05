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
      shellAliases = {
        zathura = "zathura --fork";
        ":q" = "exit";
        nvi = "nvim";
        nivm = "nvim";
        nim = "nvim";
        nvmi = "nvim";
      };

      initExtra = ''
        cpr() {
          rsync --archive -hh --partial --info=stats1,progress2 --modify-window=1 "$@"
        }

        mvr() {
          rsync --archive -hh --partial --info=stats1,progress2 --modify-window=1 --remove-source-files "$@"
        }
      '';
    };
  };
}
