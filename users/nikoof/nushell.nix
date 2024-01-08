{
  config,
  pkgs,
  ...
}: {
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
}
