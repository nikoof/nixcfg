{
  inputs,
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    inputs.nixos-hardware.nixosModules.common-pc-ssd
    inputs.nixos-hardware.nixosModules.common-gpu-nvidia-nonprime

    ./hardware.nix
    ./services/ollama.nix
    ./services/syncthing.nix
  ];

  system.stateVersion = "23.11";
  nixpkgs.hostPlatform = "x86_64-linux";

  networking = {
    hostName = "gauss";
    networkmanager.enable = true;
    firewall.enable = true;
    tempAddresses = "disabled";
    interfaces.enp3s0.wakeOnLan.enable = true;
    interfaces.enp3s0.useDHCP = true;
    nftables.enable = true;
  };

  services.zerotierone = {
    enable = true;
    joinNetworks = ["856127940c682c75"];
  };

  wm.dwm.enable = true;

  desktop = {
    enable = true;
    redshift.enable = true;
    pipewire.enable = true;

    printing.enable = true;
    printing.autodetect = true;
  };

  qt.enable = true;
  qt.style = "adwaita-dark";
  qt.platformTheme = "gnome";

  apps = {
    gns3.enable = true;

    gaming.bottles.enable = true;
    gaming.steam.enable = true;
    gaming.heroic.enable = true;
    gaming.mangohud.enable = true;

    gaming.victoria2Server.openFirewall = true;
  };

  security.nitrokey.enable = true;
  security.nitrokey.enableSSHSupport = true;
  security.keepassxc.enable = true;
  services.openssh.enable = true;
  programs.dconf.enable = true;
  hardware.wacom.enable = true;

  services.xserver.xrandrHeads = [
    {
      output = "DP-3";
      primary = true;
    }
    {
      output = "HDMI-0";
      monitorConfig = ''
        Option "RightOf" "DP-3"
      '';
    }
  ];
}
