{ stdenv
, lib
, fetchFromGitHub
, qtgraphicaleffects
, ...
}:

stdenv.mkDerivation rec {
  pname = "sddm-sugar-candy-tokyonight";
  version = "unstable-2023-06-30";

  src = fetchFromGitHub {
    owner = "Nikoof";
    repo = "sddm-sugar-candy-tokyonight";
    rev = "0eb32a21f63f8e2a1cc230b75ee87e6050085468";
    sha256 = "05k89hzj7sh6ibrr4k3a8zg4cjpidjdjvjn0h7ycbwg988vkfz9b";
  };

  propagateBuildInputs = [ qtgraphicaleffects ];

  dontBuild = true;
  dontWrapQtApps = true;

  postInstall = ''
    mkdir -p $out/share/sddm/themes/sugar-candy-tokyonight
    mv * $out/share/sddm/themes/sugar-candy-tokyonight/
  '';

  postFixup = ''
    mkdir -p $out/nix-support
    echo ${qtgraphicaleffects} >> $out/nix-support/propagated-user-env-packages
  '';

  meta = with lib; {
    description = "SDDM Sugar Candy theme with Tokyonight colors and custom background";
    homepage = "https://github.com/Nikoof/sddm-sugar-candy-tokyonight";
    license = licenses.gpl3;
    platforms = platforms.linux;
  };
}
