{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.apps.taskwarrior;
in {
  options = {
    apps.taskwarrior.enable = lib.mkEnableOption "Taskwarrior";
  };

  config = lib.mkIf cfg.enable {
    programs.taskwarrior = {
      enable = true;
      colorTheme = ./nord.theme;
      config.taskd = {
        certificate = "$XDG_DATA_HOME/task/client.cert.pem";
        key = "$XDG_DATA_HOME/task/client.key.pem";
        ca = "$XDG_DATA_HOME/task/ca.cert.pem";
        server = "home.nikoof.tech:53589";
        credentials = "Public/Nikoof/58143320-265b-43c5-b7ec-f77b5ad1669b";
      };
      extraConfig = ''
        data.location=$XDG_DATA_HOME/task/
        hooks.location=$XDG_CONFIG_HOME/task/hooks/
      '';
    };
  };
}
