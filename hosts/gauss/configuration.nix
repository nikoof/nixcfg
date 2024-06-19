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
    ./services/syncthing.nix
  ];

  system.stateVersion = "23.11";
  nixpkgs.hostPlatform = "x86_64-linux";
  boot.loader = {
    systemd-boot.enable = true;
    systemd-boot.consoleMode = "max";
    efi.canTouchEfiVariables = true;
  };

  boot.plymouth = {
    enable = true;
    themePackages = with pkgs; [nixos-bgrt-plymouth];
    theme = "nixos-bgrt";
  };

  networking = {
    hostName = "gauss";
    networkmanager.enable = true;
    firewall.enable = true;
    tempAddresses = "disabled";
    interfaces.enp3s0.wakeOnLan.enable = true;
    interfaces.enp3s0.useDHCP = true;
    nftables.enable = true;
  };

  desktop = {
    xdgDirs.enable = true;
    plasma.enable = true;

    printing.enable = true;
    printing.autodetect = true;
  };

  apps = {
    gns3.enable = true;

    gaming.bottles.enable = true;
    gaming.steam.enable = true;
    gaming.mangohud.enable = true;

    gaming.victoria2Server.openFirewall = true;
  };

  peripherals.wacom.enable = true;
  peripherals.nitrokey = {
    enable = true;
    enableSSHSupport = true;
  };

  services.ratbagd.enable = true;
  services.openssh.enable = true;
  services.zerotierone = {
    enable = true;
    joinNetworks = ["856127940c682c75"];
  };

  services.ollama = {
    enable = true;
    acceleration = "cuda";
  };

  programs.dconf.enable = true;

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
