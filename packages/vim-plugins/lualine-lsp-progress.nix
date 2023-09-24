{
  lib,
  fetchFromGitHub,
  vimUtils,
}: let
  name = "lualine-lsp-progress";
  src = fetchFromGitHub {
    owner = "arkav";
    repo = "lualine-lsp-progress";
    rev = "56842d097245a08d77912edf5f2a69ba29f275d7";
    sha256 = "sha256-8HMtydFDzTxsuKvce+bIra9vZ9zHfEBHyR346W635b8=";
  };
in
  vimUtils.buildVimPlugin {
    inherit name src;
  }
