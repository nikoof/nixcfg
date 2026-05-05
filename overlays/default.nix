{inputs, ...}: rec {
  local-packages = final: prev: {
    local = import ../packages {pkgs = final;};
  };

  modifications = final: prev: {
    pynitrokey = prev.pynitrokey.overridePythonAttrs (old: {
      dependencies = old.dependencies ++ old.optional-dependencies.pcsc;
    });

    opensslFull = prev.openssl.override {
      providers = [
        {
          name = "pkcs11";
          package = final.pkcs11-provider;
        }
      ];
    };

    bottles = prev.bottles.override {
      removeWarningPopup = true;
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

    # NOTE: nh broke search again, I'm just gonna stay on their master lmao
    nh = inputs.nh.packages.${prev.stdenv.hostPlatform.system}.default;

    # NOTE: overriding noto-fonts forces a rebuild of libreoffice
    # noto-fonts = prev.noto-fonts.override {
    #   variants = ["Noto Sans" "Noto Serif"];
    # };
  };

  unstable-packages = final: prev: {
    unstable = import inputs.nixpkgs-unstable {
      system = final.stdenv.hostPlatform.system;
      overlays = [modifications local-packages];
      inherit (prev) config;
    };
  };
}
