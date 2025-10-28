{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  cfg = config.apps.nvim;
  nvim = inputs.self.outputs.packages.${pkgs.stdenv.hostPlatform.system}.nvimFull;
in {
  imports = [];

  options = {
    apps.nvim.enable = lib.mkEnableOption "Enable nvim(Cats) config.";
  };

  config = lib.mkIf cfg.enable {
    home.sessionVariables = {
      EDITOR = "${nvim}/bin/nvim";
    };

    home.packages = [
      nvim
    ];
  };
}
