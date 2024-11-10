{
  inputs,
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
    inputs.nixos-hardware.nixosModules.common-gpu-nvidia

    inputs.lanzaboote.nixosModules.lanzaboote

    ./hardware.nix
    ./containers/arch.nix
  ];

  nix.settings.cores = 16;

  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/tomorrow-night.yaml";
    image = ../../wallpapers/moebius-bw.png;
    override = {
      base00 = "000000";
    };
  };

  stylix.opacity = {
    applications = 1.0;
    terminal = 0.8;
    desktop = 1.0;
    popups = 1.0;
  };

  stylix.fonts = {
    sizes = {
      applications = 10;
      terminal = 12;
      desktop = 10;
      popups = 10;
    };

    monospace = {
      package = pkgs.nerdfonts.override {fonts = ["FiraCode"];};
      name = "FiraCode Nerd Font Mono";
    };

    sansSerif = {
      package = pkgs.dejavu_fonts;
      name = "DejaVu Sans";
    };

    serif = {
      package = pkgs.dejavu_fonts;
      name = "DejaVu Serif";
    };
  };

  system.stateVersion = "24.05";
  nixpkgs.hostPlatform = "x86_64-linux";

  boot.loader = {
    systemd-boot.enable = lib.mkForce false;
    systemd-boot.consoleMode = "max";
    efi.canTouchEfiVariables = true;
  };

  boot.lanzaboote = {
    enable = true;
    pkiBundle = "/etc/secureboot";
  };

  hardware.bluetooth.enable = true;
  networking = {
    hostName = "hofstadter";
    tempAddresses = "disabled";
    firewall.enable = true;
    nftables.enable = true;
  };

  services.strongswan.enable = true;
  networking.networkmanager = {
    enable = true;
    enableStrongSwan = true;
  };

  users.users.nikoof = {
    description = "Nicolas Bratoveanu";
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager" "dialout" "tty" "plugdev" "uucd" "libvirtd" "optical" "cdrom" "ubridge"];
  };

  home-manager = {
    extraSpecialArgs = {inherit inputs;};
    useGlobalPkgs = true;

    users.nikoof = ./users/nikoof.nix;
  };

  services.xserver = {
    enable = true;
    displayManager.lightdm = {
      enable = true;
      greeters.slick.enable = true;
    };
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
  };

  desktop = {
    xdgDirs.enable = true;

    printing.enable = true;
    printing.autodetect = true;
  };

  apps.gns3.enable = true;

  peripherals.wacom.enable = true;
  peripherals.nitrokey = {
    enable = true;
    enableSSHSupport = true;
  };

  services.ollama = {
    enable = true;
    acceleration = "cuda";
  };

  services.openssh.enable = true;

  services.syncthing = {
    enable = true;
    user = "nikoof";
    dataDir = "/home/nikoof/Sync";
    configDir = "/home/nikoof/.config/syncthing";
  };

  services.chrony = {
    enable = true;
  };

  systemd.oomd = {
    enable = true;
  };

  programs.bash = {
    enableCompletion = true;
    enableLsColors = true;
  };

  programs.dconf.enable = true;
}
