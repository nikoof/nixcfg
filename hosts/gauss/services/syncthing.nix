{
  config,
  lib,
  inputs,
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
        "haskell" = {id = "FY2JIBO-6VYRLZD-YJBAUSF-W5CMUV7-RCXYVMU-NAKKIHT-NNZLTHA-ZHV3SAE";};
        "euler" = {id = "DFBQIQO-4Q5RHSF-TFQAH2X-7IH7URS-EQDBRHT-VAK7HAY-WXQC75W-7SOMIAO";};
      };
      folders = {
        "Obsidian" = {
          path = "/home/nikoof/Documents/nikonomicon";
          devices = ["haskell" "euler"];
        };
        "KeePass" = {
          path = "/home/nikoof/KeePass";
          devices = ["haskell" "euler"];
        };
      };
    };
  };
}
