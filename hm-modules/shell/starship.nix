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
        format = let
          enabledModules = [
            "c"
            "rust"
            "python"
          ];
        in
          pkgs.lib.concatStrings (
            ["[let](bold purple) [Γ](bold) = $username$hostname"]
            ++ map (module: "(, (\$${module}))") enabledModules
            ++ [
              " [in](bold purple) $fill ({$cmd_duration})$line_break"
              " [Γ](bold), $directory(, $git_branch) $character"
            ]
          );

        username = {
          style_user = "white bold";
          style_root = "red bold";
          format = "[$user]($style)";
          disabled = false;
          show_always = true;
        };

        directory = {
          format = "[$path]($style)[$read_only]($read_only_style)"; # no trailing space
          truncation_length = 4;
          style = "blue bold";
          read_only = "";
        };

        hostname = {
          ssh_only = false;
          ssh_symbol = "";
          style = "bold green";
          format = "[@](bold blue)[$hostname]($style)";
        };

        character = {
          format = "$symbol ";
          success_symbol = "[⊢](bold green)";
          error_symbol = "[⊢](bold red)";
          vimcmd_symbol = "[⊬](bold green)";
          vimcmd_replace_one_symbol = "[⊬](bold purple)";
          vimcmd_replace_symbol = "[⊬](bold purple)";
          vimcmd_visual_symbol = "[⊬](bold yellow)";
        };

        cmd_duration = {
          min_time = 500;
          format = "[$duration]($style)";
          style = "bold yellow";
        };

        fill = {
          symbol = " ";
        };

        git_branch = {
          format = "[$symbol$branch(:$remote_branch)]($style)"; # no trailing space
          symbol = "";
        };

        c = {
          symbol = "";
          format = "[$symbol $name-$version]($style)";
          style = "white";
          version_format = "$raw";
        };

        rust = {
          symbol = "";
          format = "[$symbol $version]($style)";
          style = "red";
        };

        python = {
          symbol = "";
          format = ''[$symbol$pyenv_prefix ($version) (\($virtualenv\))]($style)'';
          style = "yellow";
        };
      };
    };
  };
}
