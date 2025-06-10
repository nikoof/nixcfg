{
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

  buildInputs = [SDL];

  buildPhase = ''
    mkdir -p .obj
    make
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp ./sam $out/bin/sam
  '';

  meta = with lib; {
    description = "C version of sam with dynamic memory allocation";
    homepage = "https://github.com/gitRaiku/SAM";
    # License should technically be
    # license = licenses.unfreeRedistributable; # see https://github.com/s-macke/SAM#license
    platforms = platforms.all;
    mainProgram = "sam";
  };
}
