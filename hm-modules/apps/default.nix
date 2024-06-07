{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./alacritty.nix
    ./taskwarrior
    ./tmux.nix
    ./zathura.nix
    ./nvim.nix
  ];
}
