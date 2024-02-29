{
  inputs,
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [];

  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_TIME = "en_DK.UTF-8";
    LC_COLLATE = "C.UTF-8";
  };

  services.xserver = {
    layout = "ro,de";
    xkbVariant = ",qwerty";
    xkbOptions = "grp:win_space_toggle,compose:menu";
  };

  environment.systemPackages = with pkgs; [
    hunspellDicts.en_US
    hunspellDicts.en_GB-ise
  ];
}
