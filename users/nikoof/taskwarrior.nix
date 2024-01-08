{
  config,
  pkgs,
  ...
}: {
  programs.taskwarrior = {
    enable = true;
    colorTheme = ./nord.theme;
    extraConfig = ''
      data.location=$XDG_DATA_HOME/task/
      hooks.location=$XDG_CONFIG_HOME/task/hooks/
    '';
  };
}
