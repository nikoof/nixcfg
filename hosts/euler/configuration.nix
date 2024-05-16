{
  inputs,
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    inputs.nixos-hardware.nixosModules.common-pc-laptop
    inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
    inputs.nixos-hardware.nixosModules.common-gpu-nvidia

    ./hardware.nix

    ./services/ollama.nix
    ./services/power.nix
    ./services/syncthing.nix

    ./containers/ubuntu-jammy.nix
  ];

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

  system.stateVersion = "23.05";
  nixpkgs.hostPlatform = "x86_64-linux";

  hardware.bluetooth.enable = true;
  networking = {
    hostName = "euler";
    networkmanager.enable = true;
    firewall.enable = true;
    useDHCP = lib.mkDefault true;
  };

  desktop = {
    enable = true;
    redshift.enable = true;
    pipewire.enable = true;

    printing.enable = true;
    printing.autodetect = true;

    gaming = {
      enable = true;
      victoria2Server.openFirewall = true;
    };
  };

  apps = {
    gns3.enable = true;

    gaming.bottles.enable = true;
    gaming.steam.enable = true;
    gaming.heroic.enable = true;
  };

  security.nitrokey.enable = true;
  security.nitrokey.enableSSHSupport = true;
  services.openssh.enable = true;
  programs.dconf.enable = true;
  hardware.wacom.enable = true;

  virtualisation.libvirtd.enable = true;
  environment.systemPackages = with pkgs; [
    qemu
    acpi
    lm_sensors
    wireguard-tools
    wl-clipboard
  ];

  services.udev = {
    # For Pi Pico debug probe (ipw)
    extraRules = ''
      SUBSYSTEM=="usb", ATTR{idVendor}=="2e8a", ATTR{idProduct}=="000c", MODE:="666"
    '';
  };
}
