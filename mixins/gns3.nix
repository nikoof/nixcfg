{
  config,
  inputs,
  pkgs,
  lib,
  ...
}: {
  environment.systemPackages = with pkgs; [
    gns3-server
    gns3-gui

    inetutils
    ubridge
    vpcs
    qemu
    dynamips
  ];
}
