{ stdenv
, lib
, fetchFromGitHub
, qtgraphicaleffects
, qtquickcontrols
, plasma-framework
, ...
}:

stdenv.mkDerivation rec {
  pname = "nord-sddm-theme";
  version = "unstable-2022-02-03";

  src = fetchFromGitHub {
    owner = "nautilor";
    repo = "nord-sddm";
    rev = "ad72c3c7048c8aabe85bab41cbeab5f3c4502250";
    sha256 = "sha256-Ah2azTHUghxLGyAMEdR+GpEDbZGxwx2ABAyv92CxLQo=";
  };

  buildInputs = [
    qtgraphicaleffects
    qtquickcontrols
    plasma-framework
  ];

  dontBuild = true;
  dontWrapQtApps = true;

  postInstall = ''
    mkdir -p $out/share/sddm/themes
    cp -r Nord $out/share/sddm/themes/
  '';

  postFixup = ''
    mkdir -p $out/nix-support
    echo ${qtgraphicaleffects} >> $out/nix-support/propagated-user-env-packages
    echo ${plasma-framework} >> $out/nix-support/propagated-user-env-packages
  '';

  meta = with lib; {
    description = "Nord theme for SDDM";
    homepage = "https://github.com/nautilor/nord-sddm";
    platforms = platforms.linux;
  };
}
