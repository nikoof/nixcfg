{
  config,
  inputs,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./gaming.nix
    ./printing.nix
  ];

  options = {
    desktop.enable = lib.mkEnableOption "Enable desktop environment";
    desktop.redshift.enable = lib.mkEnableOption "Enable redshift";
    desktop.pipewire.enable = lib.mkEnableOption "Enable pipewire audio";
  };

  config = lib.mkMerge [
    (lib.mkIf config.desktop.enable {
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

      services.xserver = {
        enable = true;
        displayManager.sddm.enable = true;
        desktopManager.plasma5.enable = true;
      };

      environment.systemPackages = with pkgs; [
        xclip
        btop
        du-dust

        kde-gtk-config
        kcalc
        kdeconnect
        redshift-plasma-applet
        libsForQt5.sddm-kcm

        hunspellDicts.en_US
        hunspellDicts.en_GB-ise

        firefox-bin
        chromium

        nordic
        nordzy-icon-theme
        simp1e-cursors
      ];

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
    })
    (lib.mkIf config.desktop.pipewire.enable {
      security.rtkit.enable = true;
      services.pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        pulse.enable = true;
        jack.enable = true;
      };
    })
    (lib.mkIf config.desktop.redshift.enable {
      location.provider = "geoclue2";
      services.redshift = {
        enable = true;
        executable = "/bin/redshift";
      };
    })
  ];
}
