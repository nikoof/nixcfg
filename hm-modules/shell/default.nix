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

      clar = "clear";
      celar = "clear";

      zathura = "zathura --fork";
      z = "zathura --fork";
      s = "sxiv";
      pf = "ps aux | rg -i";

      nob = "./nob";
      getnob = "xh -d https://raw.githubusercontent.com/tsoding/nob.h/refs/heads/main/nob.h";

      vid = "mpv --demuxer-lavf-o=video_size=1280x720,input_format=mjpeg av://v4l2:/dev/video0 --profile=low-latency --untimed --no-osc --no-osd-bar --player-operation-mode=cplayer";
    };
  };
}
