{ inputs, ... }:

{
  local-packages = final: prev: {
    local = import ../packages { pkgs = final; };
  };

  unstable-packages = final: prev: {
    unstable = import inputs.nixpkgs-unstable {
      system = final.system;
      config.allowUnfree = true;
    };
  };
}
