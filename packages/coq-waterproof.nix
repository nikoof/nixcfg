{
  lib,
  stdenv,
  fetchFromGitHub,
  mkCoqDerivation,
  ...
}:
mkCoqDerivation {
  pname = "coq-waterproof";
  version = "2.2.0";

  src = fetchFromGitHub {
    owner = "impermeable";
    repo = "coq-waterproof";
    rev = "e82f68c9c6f3c060fa92f62f84da59a3f5367dce";
    hash = "sha256-+4dkC3MbEWH95Ek4eB/WvCnwOWcBhzvICHGXsxOn18Q=";
  };

  mlPlugin = true;
}
