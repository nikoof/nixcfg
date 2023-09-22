{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  fonts.fonts = with pkgs; [
    fira-code
    nerdfonts
    corefonts
    noto-fonts
    noto-fonts-emoji
    noto-fonts-cjk-sans
  ];
}
