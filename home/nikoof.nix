{
  config,
  inputs,
  pkgs,
  ...
}: {
  imports = [
    inputs.self.outputs.homeManagerModules.default
  ];

  home.username = "nikoof";
  home.homeDirectory = "/home/nikoof";

  devel.git.enable = true;
  devel.git.signing = true;
  devel.git.github.enable = true;

  terminal.enable = true;
  terminal.shell.nushell.enable = true;
  terminal.shell.starship.enable = true;

  devel.languages = {
    cpp.enable = true;
    rust.enable = true;
    haskell.enable = true;
    nix.enable = true;
    python.enable = true;
  };

  profiles = {
    school.enable = true;
    entertainment.enable = true;
    media.enable = true;
    productivity.enable = true;
  };

  apps = {
    zathura.enable = true;
    taskwarrior.enable = true;
    tmux.enable = true;
    nvim.enable = true;
  };

  programs.direnv.enable = true;

  home.packages = with pkgs; [
    virt-manager
  ];

  home.stateVersion = "23.05";
}
