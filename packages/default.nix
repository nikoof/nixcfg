{pkgs, ...}: {
  lunar-client = pkgs.callPackage ./lunar-client.nix {};
  vimPlugins = {
    lualine-lsp-progress = pkgs.callPackage ./vim-plugins/lualine-lsp-progress.nix {};
  };
}
