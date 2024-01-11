{
  inputs,
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ../mixins/common.nix
    ../mixins/boot.nix

    ../mixins/i18n.nix
    ../mixins/sshd.nix

    ../mixins/nikoof.nix
  ];

  nix.settings.experimental-features = ["nix-command" "flakes"];

  environment.systemPackages = with pkgs; [
    curl
    git
    gnupg
    pinentry
    playerctl
    neovim
    cifs-utils
    pciutils
    usbutils
  ];
}
