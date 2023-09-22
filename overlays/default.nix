{inputs, ...}: {
  local-packages = final: prev: {
    local = import ../packages {pkgs = final;};
  };

  modifications = final: prev: {
    graphite-gtk-theme = prev.graphite-gtk-theme.override {
      tweaks = ["nord"];
    };
  };

  unstable-packages = final: prev: {
    unstable = import inputs.nixpkgs-unstable {
      system = final.system;
      config.allowUnfree = true;
    };
  };
}
