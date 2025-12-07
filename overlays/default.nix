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

    mathematica = prev.mathematica.override {
      source = final.requireFile {
        name = "Wolfram_14.3.0_LIN.sh";
        # Get this hash via a command similar to this:
        # nix-store --query --hash $(nix store add-path Mathematica_XX.X.X_BNDL_LINUX.sh --name 'Mathematica_XX.X.X_BNDL_LINUX.sh')
        sha256 = "18a8wf9j6n4dp631psa254hyniklm61i3qqrvvnz1sgfvd1hl6cf";
        message = ''
          Override for Mathematica includes a different src for the installer, and it is missing.
        '';
        hashMode = "recursive";
      };
    };
  };

  unstable-packages = final: prev: {
    unstable = import inputs.nixpkgs-unstable {
      system = final.system;
      overlays = [modifications];
      config.allowUnfree = true;
    };
  };
}
