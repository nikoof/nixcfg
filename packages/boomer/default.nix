{
  lib,
  buildNimPackage,
  fetchFromGitHub,
  libX11,
  libXrandr,
  libGL,
  libXext,
  ...
}:
buildNimPackage {
  pname = "boomer";
  version = "unstable-2024-02-08";
  src = fetchFromGitHub {
    owner = "tsoding";
    repo = "boomer";
    rev = "dfd4e1f5514e2a9d7c7a6429c1c0642c2021e792";
    hash = "sha256-o65/VVxttriA5Qqt35lLKkWIZYS7T4VBBuYdAIGUmx8=";
  };
  buildInputs = [libX11 libXrandr libGL libXext];
  nimbleFile = "boomer.nimble";
  lockFile = ./lock.json;
  nimFlags =
    # NOTE: None of these actually work properly
    # See: https://github.com/tsoding/boomer/issues/109
    lib.optional false [
      "-d:live"
      "-d:mitshm"
      "-d:select"
    ];

  # INFO: See https://github.com/tsoding/boomer/issues/26#issuecomment-1820374229
  patchPhase = ''
    find . -type f -name "*.nim" -exec sed -i 's/TXShmSegmentInfo/PXShmSegmentInfo/g' {} +
  '';

  meta = with lib; {
    description = "Zoomer application for Linux";
    homepage = "https://github.com/tsoding/boomer";
    license = licenses.mit;
    mainProgram = "boomer";
  };
}
