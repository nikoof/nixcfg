{
  pkgs,
  inputs,
  ...
}: {
  programs.git = {
    enable = true;
    userName = "Nicolas Bratoveanu";
    userEmail = "nicolasbratoveanu@proton.me";

    extraConfig = {
      safe = {directory = "/etc/nixos";};
    };

    signing = {
      signByDefault = true;
      key = "E9D147D0D897E66F";
    };
  };

  programs.gh = {
    enable = true;
    enableGitCredentialHelper = true;
  };
}
