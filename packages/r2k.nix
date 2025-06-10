{
  lib,
  stdenv,
  fetchFromGitHub,
  wayland,
  wlroots_0_18,
  wayland-protocols,
  libxkbcommon,
  libX11,
  libXft,
  xwayland,
  freetype,
}:
stdenv.mkDerivation {
  pname = "r2k";
  version = "1.4";

  src = fetchFromGitHub {
    owner = "gitRaiku";
    repo = "r2k";
    rev = "v1.4";
    hash = "sha256-K11xrgS3NgRn2GFVvtY9iTchz2QdhtCp2dRXL0tlNro=";
  };

  buildInputs = [
    wayland
    wlroots_0_18
    wayland-protocols
    libxkbcommon
    xwayland
    libX11
    libXft
    freetype
  ];

  buildPhase = ''
    substituteInPlace ./src/defs.h \
      --replace "/usr/share" "$out/share"
    make
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp bin/r2k $out/bin/r2k
    cp bin/r2kd $out/bin/r2kd
    mkdir -p $out/share/r2k
    cp resources/RaikuDict $out/share/r2k/r2kdict
  '';

  meta = with lib; {
    description = "Romaji to kana input utility";
    homepage = "https://github.com/gitRaiku/r2k";
    platforms = platforms.linux;
    mainProgram = "r2k";
  };
}
