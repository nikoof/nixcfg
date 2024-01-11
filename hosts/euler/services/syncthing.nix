{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  services.syncthing = {
    enable = true;
    user = "nikoof";
    dataDir = "/home/nikoof/Sync";
    configDir = "/home/nikoof/.config/syncthing";
    overrideDevices = true;
    overrideFolders = true;
    settings = {
      devices = {
        "gauss" = {id = "WR7JF54-XLCUEQQ-TY2T2AQ-TRRK5U5-MKIU765-ZXDBJHM-APZKZFO-SM6C3QN";};
        "haskell" = {id = "FY2JIBO-6VYRLZD-YJBAUSF-W5CMUV7-RCXYVMU-NAKKIHT-NNZLTHA-ZHV3SAE";};
      };
      folders = {
        "Obsidian" = {
          path = "/home/nikoof/Documents/nikonomicon";
          devices = ["haskell" "gauss"];
        };
        "KeePass" = {
          path = "/home/nikoof/KeePass";
          devices = ["haskell" "gauss"];
        };
      };
    };
  };
}
