{inputs, ...}: rec {
  local-packages = final: prev: {
    local = import ../packages {pkgs = final;};
  };

  modifications = final: prev: {
    graphite-gtk-theme = prev.graphite-gtk-theme.override {
      tweaks = ["nord"];
    };

    ciscoPacketTracer8 = prev.ciscoPacketTracer8.overrideAttrs {
      desktopItems = [
        (prev.makeDesktopItem {
          name = "cisco-pt8.desktop";
          desktopName = "Cisco Packet Tracer 8";
          icon = "ciscoPacketTracer8";
          exec = "env XDG_CURRENT_DESKTOP=GNOME packettracer8 %f";
          mimeTypes = ["application/x-pkt" "application/x-pka" "application/x-pkz"];
        })
      ];
    };

    # NOTE: overriding noto-fonts forces a rebuild of libreoffice
    # noto-fonts = prev.noto-fonts.override {
    #   variants = ["Noto Sans" "Noto Serif"];
    # };
  };

  unstable-packages = final: prev: {
    unstable = import inputs.nixpkgs-unstable {
      system = final.system;
      overlays = [modifications];
      config.allowUnfree = true;
    };
  };
}
