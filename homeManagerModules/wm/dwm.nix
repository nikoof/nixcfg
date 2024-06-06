{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  cfg = config.wm.dwm;
in {
  imports = [];

  options.wm.dwm = {
    enable = lib.mkEnableOption "Enable dwm";
    package = lib.mkPackageOption pkgs "dwm" {};
    wallpaper = with lib;
      mkOption {
        type = types.path;
        default = null;
      };
  };

  config = lib.mkIf cfg.enable {
    services.picom = {
      enable = true;
    };

    home.packages = lib.mkIf (cfg.wallpaper != null) [pkgs.feh];
    xdg.dataFile."fehbg".source = cfg.wallpaper;
    xdg.dataFile."dwm/autostart.sh" = {
      text =
        builtins.concatStringsSep "\n" ([]
          ++ lib.lists.optional (cfg.wallpaper != null) ''feh --no-fehbg --bg-scale "$XDG_DATA_HOME/fehbg"'');
      executable = true;
    };
  };
}
