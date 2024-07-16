{
  callPackage,
  lib,
  stdenv,
  fetchFromGitHub,
  SDL,
}:
stdenv.mkDerivation {
  pname = "SAM";
  version = "0.69.0";

  src = fetchFromGitHub {
    owner = "s-macke";
    repo = "SAM";
    rev = "2cb53fbdbba78f2943e5ab6d4569d5e0a056e3ad";
    sha256 = "sha256-0xDbowVyYz0Y7NtxpSH0ao1KVhuOQ6JWTY6qBv+gkho=";
  };

  buildInputs = [
    SDL
  ];

  installPhase = ''
    mkdir -p $out/bin
    cp ./sam $out/bin/sam
  '';
}
