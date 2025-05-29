{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./alacritty.nix
    ./kitty.nix
    ./taskwarrior
    ./tmux.nix
    ./zathura.nix
    ./nvim.nix
  ];
}
