with import <nixpkgs> {};

stdenv.mkDerivation rec {
  pname = "sddm-sugar-candy-tokyonight";
  version = "unstable-2023-06-30";

  src = fetchFromGitHub {
    owner = "Nikoof";
    repo = "sddm-sugar-candy-tokyonight";
    rev = "58e190db4821e2fe95ebb29e66e6af67c8d6de4b";
    sha256 = "05k89hzj7sh6ibrr4k3a8zg4cjpidjdjvjn0h7ycbwg988vkfz9b";
  };

  dontBuild = true;

  installPhase = ''
    mkdir -p $out/share/sddm/themes
    cp -aR $src $out/share/sddm/themes/sugar-candy-tokyonight
  '';

  meta = with lib; {
    description = "SDDM Sugar Candy theme with Tokyonight colors and custom background";
    homepage = "https://github.com/Nikoof/sddm-sugar-candy-tokyonight";
    license = licenses.gpl3;
    platforms = platforms.linux;
  };
}
