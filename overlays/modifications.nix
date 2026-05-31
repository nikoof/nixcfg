final: prev: {
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

  # NOTE: overriding noto-fonts forces a rebuild of libreoffice
  # noto-fonts = prev.noto-fonts.override {
  #   variants = ["Noto Sans" "Noto Serif"];
  # };
}
