{
  lib,
  python,
  fetchPypi,
}:
python.pkgs.buildPythonApplication rec {
  pname = "tockloader";
  version = "1.9.0";

  src = fetchPypi {
    inherit pname version;
    hash = "sha256-7W55jugVtamFUL8N3dD1LFLJP2UDQb74V6o96rd/tEg=";
  };

  propagatedBuildInputs = with python.pkgs; [
    argcomplete
    colorama
    crcmod
    pycryptodome
    pyserial
    questionary
    toml
    tqdm
    ecdsa
  ];

  # Project has no test suite
  checkPhase = ''
    runHook preCheck
    $out/bin/tockloader --version | grep -q ${version}
    runHook postCheck
  '';
}
