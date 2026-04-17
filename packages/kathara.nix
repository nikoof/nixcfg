{
  lib,
  fetchFromGitHub,
  buildPythonApplication,
  setuptools,
  binaryornot,
  docker,
  kubernetes,
  requests,
  pyroute2,
  rich,
  fs,
  chardet,
  libtmux,
}:
buildPythonApplication {
  pname = "kathara";
  version = "3.8.1";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "KatharaFramework";
    repo = "Kathara";
    rev = "ff6a94ef2bc2918a8973e7703866312833dc54f5";
    hash = "sha256-e6o+9xai4ac+gRuVEMU6OguONsojC5r3gwbJ9SCCb98=";
  };

  # Hack to make this builder wrap the entrypoint because
  # upstream doesn't declare any entrypoints in pyproject.toml
  postInstall = ''
    mkdir $out/bin
    cp $out/lib/python3.13/site-packages/kathara.py $out/bin/kathara
    chmod +x $out/bin/kathara
  '';

  build-system = [
    setuptools
  ];

  dependencies = [
    binaryornot
    docker
    kubernetes
    requests
    pyroute2
    rich
    setuptools
    fs
    chardet
    libtmux
  ];
}
