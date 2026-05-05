{
  pkgs,
  lib,
  stdenv,
  fetchFromGitHub,
  SDL2,
  pkg-config,
}:
stdenv.mkDerivation {
  pname = "sowon";
  version = "0.1.0";

  src = fetchFromGitHub {
    owner = "tsoding";
    repo = "sowon";
    rev = "558f125a78e8ede33e86c4ec584f87892a8ab94a";
    sha256 = "sha256-QloqwVkjZwsDtxZTaywVgKuKJsyBNpcKPjKHvw9Vql8=";
  };

  buildInputs = [SDL2];
  nativeBuildInputs = [pkg-config];

  installPhase = ''
    export PREFIX=$out
    make install
  '';

  meta = with lib; {
    description = "Starting Soon Timer for Tsoding Streams";
    homepage = "https://github.com/tsoding/sowon";
    platforms = platforms.all;
    mainProgram = "sowon";
  };
}
