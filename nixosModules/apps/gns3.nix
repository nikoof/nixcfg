{
  config,
  inputs,
  pkgs,
  lib,
  ...
}: let
  cfg = config.apps.gns3;
in {
  options.apps.gns3 = {
    enable = lib.mkEnableOption "Enable GNS3 with local server";
  };

  config = lib.mkIf cfg.enable {
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
  };
}
