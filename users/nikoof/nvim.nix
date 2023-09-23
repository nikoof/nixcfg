{
  config,
  pkgs,
  ...
}: {
  programs.nixvim = {
    enable = true;

    colorschemes.nord.enable = true;

    plugins = {
      lualine.enable = true;
      comment-nvim.enable = true;

      cmp-nvim-lsp.enable = true;
    };

    plugins.lsp = {
      enable = true;
      servers = {
        rnix-lsp.enable = true;
        rust-analyzer.enable = true;
      };
    };

    plugins.nvim-cmp = {
      enable = true;
      mapping = {
        "<CR>" = "cmp.mapping.confirm({ select = true })";
        "<Tab>" = {
          action = ''
            function(fallback)
              if cmp.visible() then
                cmp.select_next_item()
              elseif luasnip.expandable() then
                luasnip.expand()
              elseif luasnip.expand_or_jumpable() then
                luasnip.expand_or_jump()
              elseif check_backspace() then
                fallback()
              else
                fallback()
              end
            end
          '';
          modes = [
            "i"
            "s"
          ];
        };
      };
    };
  };
}
