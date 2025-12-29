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
    ./tmux.nix
    ./zathura.nix
    ./sioyek.nix
    ./nvim.nix
  ];
}
