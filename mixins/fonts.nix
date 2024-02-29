{
  config,
  inputs,
  pkgs,
  lib,
  ...
}: {
  fonts.packages = with pkgs; [
    fira-code
    nerdfonts
    corefonts
    noto-fonts
    noto-fonts-emoji
    noto-fonts-cjk-sans
    charis-sil
    seshat
    open-sans
    gelasio
  ];
}
