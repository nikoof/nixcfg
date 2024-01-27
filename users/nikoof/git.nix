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
      push = {autoSetupRemote = true;};
    };

    signing = {
      signByDefault = true;
      key = "94B9F744D3E82C46";
    };
  };

  programs.gh = {
    enable = true;
    gitCredentialHelper.enable = true;
  };
}
