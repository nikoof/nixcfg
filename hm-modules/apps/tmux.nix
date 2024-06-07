{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.apps.tmux;
in {
  options = {
    apps.tmux.enable = lib.mkEnableOption "tmux";
  };

  config = lib.mkIf cfg.enable {
    programs.tmux = {
      enable = true;
      keyMode = "vi";
    };
  };
}
