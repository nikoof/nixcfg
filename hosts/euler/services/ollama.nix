{
  config,
  lib,
  inputs,
  pkgs,
  ...
}: {
  services.ollama = {
    enable = true;
    acceleration = "cuda";
  };
}
