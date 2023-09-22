{ inputs, config, lib, ... }:

{
  imports = [ ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nixpkgs.config.allowUnfree = true;

  environment.sessionVariables = rec {
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_STATE_HOME = "$HOME/.local/state";
    XDG_CACHE_HOME = "$HOME/.cache";

    XDG_BIN_HOME = "$HOME/.local/state";
    PATH = [ "${XDG_BIN_HOME}" ];

    RUSTUP_HOME = "${XDG_DATA_HOME}/rustup";
    GTK2_RC_FILES = "${XDG_CONFIG_HOME}/gtk-2.0/gtkrc";
    HISTFILE = "${XDG_STATE_HOME}/bash/history";
    RANDFILE = "${XDG_STATE_HOME}/rnd";
    CUDA_CACHE_PATH = "${XDG_CACHE_HOME}/nv";
    XCOMPOSECACHE = "${XDG_CACHE_HOME}/X11/xcompose";
    SQLITE_HISTORY = "${XDG_CACHE_HOME}/sqlite_history";
    STACK_XDG = "1";

    GTK_IM_MODULE = "xim";
  };
}
