{
  pkgs,
  lib,
  stdenv,
  fetchFromGitHub,
  makeWrapper,
  dmenu ? pkgs.dmenu,
  file,
  xdg-utils,
}:
stdenv.mkDerivation {
  pname = "dfm";
  version = "1.2";

  src = fetchFromGitHub {
    owner = "amarz45";
    repo = "dfm";
    rev = "c57f9c60a1e010d4f0a278a603e9fdf6a71c697a";
    sha256 = "0k3k19ljlh70ab1wijhgiz5cs26v02fsfdlwymliz2ridzwkcj0r";
  };

  buildInputs = [dmenu file xdg-utils];
  nativeBuildInputs = [makeWrapper];

  buildPhase = " ";
  installPhase = ''
    mkdir -p $out/bin
    cp dfm $out/bin/dfm
    wrapProgram $out/bin/dfm \
      --prefix PATH : ${lib.makeBinPath [dmenu]}
  '';

  meta = with lib; {
    description = "Manage files using dmenu";
    homepage = "https://github.com/amarz45/dfm";
    platforms = platforms.all;
    mainProgram = "dfm";
  };
}
