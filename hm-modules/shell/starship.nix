{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.shell.starship;
in {
  options.shell.starship = {
    enable = lib.mkEnableOption "Enable Starship prompt";
  };

  config = lib.mkIf cfg.enable {
    programs.starship = {
      enable = true;
      enableBashIntegration = lib.mkDefault config.shell.bash.enable;
      enableNushellIntegration = lib.mkDefault config.shell.nushell.enable;
      enableFishIntegration = lib.mkDefault config.shell.fish.enable;
      settings = {
        format = pkgs.lib.concatStrings [
          "[∴ ](bold blue)"
          "$hostname"
          "$directory"
          "([∴](bold blue) ($c) )"
          "([∴](bold blue) ($rust) )"
          "([∴](bold blue) ($python))"
          " $fill "
          ''(\[$cmd_duration\])''
          "$line_break"
          ''\[$username\]''
          " $character"
          "[λ](bold blue) "
        ];

        username = {
          style_user = "white bold";
          style_root = "red bold";
          format = "[$user]($style)";
          disabled = false;
          show_always = true;
        };

        directory = {
          truncation_length = 4;
          style = "blue bold";
          read_only = " ";
        };

        hostname = {
          ssh_only = false;
          ssh_symbol = "";
          style = "bold green";
          format = "[@](bold blue)[$hostname]($style)[ ∴ ](bold blue)";
        };

        character = {
          format = "$symbol ";
          success_symbol = "[~>](bold green)";
          error_symbol = "[~>](bold red)";
        };

        cmd_duration = {
          min_time = 500;
          format = "[$duration]($style)";
          style = "bold yellow";
        };

        fill = {symbol = " ";};

        c = {
          symbol = "";
          format = "[$symbol $name-$version]($style)";
          style = "bold";
          version_format = "$raw";
        };

        rust = {
          symbol = "";
          format = "[$symbol $version]($style)";
          style = "red bold";
        };

        python = {
          symbol = "";
          format = ''[$symbol$pyenv_prefix ($version) (\($virtualenv\))]($style)'';
          style = "yellow bold";
        };
      };
    };
  };
}
