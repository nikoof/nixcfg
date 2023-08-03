{ stdenv
, lib
, fetchFromGitHub
, qtgraphicaleffects
, ...
}:

stdenv.mkDerivation rec {
  pname = "nord-sddm-theme";
  version = "unstable-2022-02-03";

  src = fetchFromGitHub {
    owner = "nautilor";
    repo = "nord-sddm";
    rev = "ad72c3c7048c8aabe85bab41cbeab5f3c4502250";
    sha256 = "05k89hzj7sh6ibrr4k3a8zg4cjpidjdjvjn0h7ycbwg988vkfz9b";
  };

  # propagatedBuildInputs = [ qtgraphicaleffects ];

  dontBuild = true;
  dontWrapQtApps = true;

  postInstall = ''
    mv Nord $out/share/sddm/themes/
  '';

  meta = with lib; {
    description = "Nord theme for SDDM";
    homepage = "https://github.com/nautilor/nord-sddm";
    license = licenses.none;
    platforms = platforms.linux;
  };
}
