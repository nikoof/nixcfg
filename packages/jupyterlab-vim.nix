{
  lib,
  buildPythonPackage,
  fetchPypi,
  jupyterlab,
}:
buildPythonPackage rec {
  pname = "jupyterlab-vim";
  version = "4.1.4";
  format = "wheel";

  src = fetchPypi {
    inherit version format;
    pname = "jupyterlab_vim";
    python = "py3";
    dist = "py3";
    hash = "sha256-E8Kf8f04X93zPrhaixZEG+acYgMocLuXowVLLPAKsTU=";
  };

  propagatedBuildInputs = [
    jupyterlab
  ];

  meta = with lib; {
    description = "Vim notebook cell bindings for JupyterLab";
    homepage = "https://jupyterlab-contrib.github.io/jupyterlab-vim.html";
    license = licenses.mit;
    platforms = platforms.all;
  };
}
