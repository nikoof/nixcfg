{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.apps.kitty;
in {
  options.apps.kitty = {
    enable = lib.mkEnableOption "Enable kitty";
  };

  config = lib.mkIf cfg.enable {
    programs.kitty = {
      enable = true;
      shellIntegration.mode = "no-cursor";
      shellIntegration.enableFishIntegration = true;
      enableGitIntegration = true;

      settings = {
        paste_actions = "quote-urls-at-prompt,replace-dangerous-control-codes";
      };
    };
  };
}
