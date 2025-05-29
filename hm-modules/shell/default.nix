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
    ./fish.nix
    ./nushell.nix
    ./starship.nix
  ];

  options.shell = {
    enable = lib.mkEnableOption "Enable general shell config";
  };

  config = lib.mkIf cfg.enable {
    programs.eza = {
      enable = true;
      enableBashIntegration = lib.mkDefault config.shell.bash.enable;
      enableFishIntegration = lib.mkDefault config.shell.fish.enable;
      extraOptions = [
        "--group-directories-first"
        "--header"
      ];
    };

    programs.zoxide = {
      enable = true;
      enableBashIntegration = lib.mkDefault config.shell.bash.enable;
      enableFishIntegration = lib.mkDefault config.shell.fish.enable;
      options = [
        "--cmd cd"
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

      nob = "./nob";
    };
  };
}
