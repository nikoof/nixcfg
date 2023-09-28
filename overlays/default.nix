{inputs, ...}: {
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
  };

  unstable-packages = final: prev: {
    unstable = import inputs.nixpkgs-unstable {
      system = final.system;
      config.allowUnfree = true;
    };
  };
}
