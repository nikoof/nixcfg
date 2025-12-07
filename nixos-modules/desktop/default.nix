{
  config,
  inputs,
  pkgs,
  lib,
  ...
}: let
  cfg = config.desktop;
in {
  imports = [
    ./wm
  ];

  options.desktop = {
    redshift.enable = lib.mkEnableOption "Enable redshift.";
    xdgDirs.enable = lib.mkEnableOption "Set environment variables for XDG Base Directory Specification.";
  };

  config = {
    environment.systemPackages = with pkgs; [
      # TODO: Figure out how to conditionally install X11-related programs.
      # I currently don't use Wayland at all so it's not imperative.
      xclip

      keepassxc
    ];

    fonts.packages = with pkgs; [
      fira-code
      nerd-fonts.fira-code
      corefonts
      noto-fonts
      noto-fonts-color-emoji
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
      layout = "ro,us";
      variant = ",colemak_dh";
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

    # XXX: Is this needed?
    environment.sessionVariables = lib.mkIf cfg.xdgDirs.enable rec {
      XDG_DATA_HOME = "$HOME/.local/share";
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_STATE_HOME = "$HOME/.local/state";
      XDG_CACHE_HOME = "$HOME/.cache";

      XDG_BIN_HOME = "$HOME/.local/state";
      PATH = ["${XDG_BIN_HOME}"];

      RUSTUP_HOME = "${XDG_DATA_HOME}/rustup";
      CARGO_HOME = "${XDG_DATA_HOME}/cargo";
      GTK2_RC_FILES = "${XDG_CONFIG_HOME}/gtk-2.0/gtkrc";
      HISTFILE = "${XDG_STATE_HOME}/bash/history";
      RANDFILE = "${XDG_STATE_HOME}/rnd";
      CUDA_CACHE_PATH = "${XDG_CACHE_HOME}/nv";
      XCOMPOSECACHE = "${XDG_CACHE_HOME}/X11/xcompose";
      SQLITE_HISTORY = "${XDG_CACHE_HOME}/sqlite_history";
      STACK_XDG = "1";
      JUPYTER_CONFIG_DIR = "${XDG_CONFIG_HOME}/jupyter";
    };
  };
}
