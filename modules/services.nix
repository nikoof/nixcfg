{ inputs, config, lib, ... }:

{
  imports = [];

  services.automatic-timezoned.enable = true;

  # OpenSSH + GPG SSH Agent
  services.openssh.enable = true;
  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  programs.dconf.enable = true;
}
