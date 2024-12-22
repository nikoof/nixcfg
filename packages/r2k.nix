{
  callPackage,
  lib,
  stdenv,
  fetchFromGitHub,
  wayland,
  wlroots_0_18,
  wayland-protocols,
  libxkbcommon,
  xorg,
  xwayland,
  freetype,
}:
stdenv.mkDerivation {
  pname = "r2k";
  version = "1.2";

  meta = with lib; {
    description = "Romaji to kana input utility";
    homepage = "https://github.com/gitRaiku/r2k";
    license = licenses.gpl2;
    platforms = platforms.linux;
    mainProgram = "r2k";
    # maintainers = [ maintainers.Raiku ];
  };

  src = fetchFromGitHub {
    owner = "gitRaiku";
    repo = "r2k";
    rev = "v1.2";
    hash = "sha256-AwY2dPLhN28UQK+u6MMuwxskXdpLg8dWPMY3YhWrfdo=";
  };

  buildInputs = [
    wayland
    wlroots_0_18
    wayland-protocols
    libxkbcommon
    xwayland
    xorg.libX11
    xorg.libXft
    freetype
  ];

  buildPhase = ''
    substituteInPlace ./src/defs.h \
      --replace "/usr/share" "$out/share"
    make
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp ./r2k $out/bin/r2k
    cp ./r2kd $out/bin/r2kd
    mkdir -p $out/share/r2k
    cp ./dict/RaikuDict $out/share/r2k/r2kdict
  '';
}
