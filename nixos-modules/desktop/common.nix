{
  config,
  inputs,
  pkgs,
  lib,
  ...
}: let
  cfg = config.desktop.common;
in {
  options.desktop.common = {
    redshift.enable = lib.mkEnableOption "Redshift";
  };

  config = {
    environment.systemPackages = with pkgs; [
      xclip
      btop
      du-dust

      firefox
      keepassxc
    ];

    fonts.packages = with pkgs; [
      fira-code
      (nerdfonts.override {fonts = ["FiraCode"];})
      corefonts
      noto-fonts
      noto-fonts-emoji
      noto-fonts-cjk-sans
      charis-sil
      seshat
      open-sans
      gelasio
      alegreya
      alegreya-sans
      cm_unicode
    ];

    i18n.defaultLocale = "en_US.UTF-8";
    i18n.extraLocaleSettings = {
      LC_TIME = "en_DK.UTF-8";
      LC_COLLATE = "C.UTF-8";
    };

    services.xserver = {
      autoRepeatDelay = 200;
      autoRepeatInterval = 20;
    };
    services.xserver.xkb = {
      layout = "ro";
      variant = "";
      options = "grp:win_space_toggle,compose:menu,caps:swapescape";
    };

    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
    };

    location.provider = lib.mkIf cfg.redshift.enable "geoclue2";
    services.redshift = lib.mkIf cfg.redshift.enable {
      enable = true;
      executable = "/bin/redshift";
    };
  };
}
