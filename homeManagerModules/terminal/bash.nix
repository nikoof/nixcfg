{
  config,
  pkgs,
  lib,
  ...
}: {
  options = {
    terminal.shell.bash.enable = lib.mkEnableOption "Enable bash";
  };

  config = lib.mkIf config.terminal.shell.bash.enable {
    programs.bash = {
      enable = true;
      shellAliases = rec {
        ip = "ip --color=auto";
        zathura = "zathura --fork";
      };

      initExtra = ''
        cpr() {
          rsync --archive -hh --partial --info=stats1,progress2 --modify-window=1 "$@"
        }

        mvr() {
          rsync --archive -hh --partial --info=stats1,progress2 --modify-window=1 --remove-source-files "$@"
        }

        task
      '';
    };
  };
}
