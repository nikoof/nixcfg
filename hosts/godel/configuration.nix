{
  inputs,
  config,
  pkgs,
  lib,
  ...
}: {
  environment.systemPackages = with pkgs; [
    hello
  ];

  services.openssh.enable = true;
}
