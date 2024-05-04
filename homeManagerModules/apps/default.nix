{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./taskwarrior
    ./tmux.nix
    ./zathura.nix
  ];
}
