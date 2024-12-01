{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  cfg = config.shell;
in {
  imports = [
    ./bash.nix
    ./nushell.nix
    ./starship.nix
  ];

  options.shell = {
    enable = lib.mkEnableOption "Enable general shell config";
  };

  config = lib.mkIf cfg.enable {
    programs.eza = {
      enable = true;
      enableBashIntegration = true;
      extraOptions = [
        "--group-directories-first"
        "--header"
      ];
    };

    home.shellAliases = {
      cpr = "rsync --archive -hh --partial --info=stats1,progress2 --modify-window=1";
      mvr = "rsync --archive -hh --partial --info=stats1,progress2 --modify-window=1 --remove-source-files";

      ":q" = "exit";
      nvi = "nvim";
      nivm = "nvim";
      nim = "nvim";
      nvmi = "nvim";

      zathura = "zathura --fork";
    };
  };
}
