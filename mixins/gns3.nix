{
  config,
  inputs,
  pkgs,
  lib,
  ...
}: {
  virtualisation.libvirtd.enable = true;

  users.groups.ubridge = {};
  security.wrappers.ubridge = {
    source = "/run/current-system/sw/bin/ubridge";
    capabilities = "cap_net_admin,cap_net_raw=ep";
    owner = "root";
    group = "ubridge";
    permissions = "u+rx,g+x";
  };

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
