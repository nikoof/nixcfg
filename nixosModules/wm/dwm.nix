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
    enable = lib.mkEnableOption "dwm";
  };

  config = lib.mkIf cfg.enable {
    services.xserver.windowManager.dwm = {
      enable = true;
      package = inputs.dwm.defaultPackage.${pkgs.stdenv.hostPlatform.system};
    };

    environment.systemPackages = with pkgs; [
      dmenu
    ];
  };
}
