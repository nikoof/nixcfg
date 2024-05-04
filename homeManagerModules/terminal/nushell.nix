{
  config,
  pkgs,
  lib,
  ...
}: {
  options = {
    terminal.shell.nushell.enable = lib.mkEnableOption "Enable nushell";
  };

  config = lib.mkIf config.terminal.shell.nushell.enable {
    programs.nushell = {
      enable = true;
      configFile.text = ''
        $env.config = {
          show_banner: false,
        }
      '';

      shellAliases = {
        ip = "ip --color=auto";
        zathura = "zathura --fork";
        cpr = "rsync --archive -hh --partial --info=stats1,progress2 --modify-window=1";
        mvr = "rsync --archive -hh --partial --info=stats1,progress2 --modify-window=1 --remove-source-files";
      };
    };

    # programs.carapace = {
    #   enable = true;
    #   enableNushellIntegration = true;
    # };
  };
}
