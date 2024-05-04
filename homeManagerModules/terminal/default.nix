{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./alacritty.nix
    ./bash.nix
    ./nushell.nix
    ./starship.nix
  ];

  config = {
    terminal.enable = lib.mkDefault true;
    terminal.shell.bash.enable = lib.mkDefault true;
    terminal.shell.starship.enable = lib.mkDefault true;
  };
}
