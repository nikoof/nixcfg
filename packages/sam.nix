{
  callPackage,
  lib,
  stdenv,
  fetchFromGitHub,
  SDL,
}:
stdenv.mkDerivation {
  pname = "sam";
  version = "raiku";

  src = fetchFromGitHub {
    owner = "gitRaiku";
    repo = "SAM";
    rev = "74bb8d401368961be268a47e2aa2664f8639daba";
    sha256 = "sha256-8R+pWHxnFblfVLohyByO8uGav2EIL42HgPBQ51YncHE=";
  };

  buildInputs = [
    SDL
  ];

  buildPhase = ''
    mkdir -p .obj
    make
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp ./sam $out/bin/sam
  '';

  meta = {
    description = "C version of sam with dynamic memory allocation";
    mainProgram = "sam";
  };
}
