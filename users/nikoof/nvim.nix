{
  config,
  pkgs,
  ...
}: {
  programs.nixvim = {
    enable = true;

    globals = {
      mapleader = " ";
      maplocalleader = " ";
    };

    options = {
      relativenumber = true;
      clipboard = "unnamedplus";
      signcolumn = "yes";
      tabstop = 4;
      softtabstop = 4;
      shiftwidth = 4;
      mouse = "a";
      termguicolors = true;
      updatetime = 300;
    };

    colorschemes.nord.enable = true;
    plugins.comment-nvim.enable = true;

    plugins.lualine = {
      enable = true;
      iconsEnabled = true;
      theme = "nord";

      sections.lualine_c = [ "lsp_progress" ];

      extensions = [
        "fzf"
	"trouble"
      ];
    };

    plugins.lsp = {
      enable = true;
      servers = {
	lua-ls.enable = true;
	pyright.enable = true;
        rnix-lsp.enable = true;
        rust-analyzer.enable = true;
      };

      keymaps.lspBuf = {
	"<leader>r" = "rename";
	"<leader>a" = "code_action";

	"gi" = "implementation";
	"gr" = "references";
	"gd" = "definition";
	"gD" = "declaration";
	"<leader>D" = "type_definition";

        "K" = "hover";
      };
    };

    plugins.lspkind = {
      enable = true;
      cmp.enable = true;
    };

    plugins.lsp-lines.enable = true;

    plugins.cmp-nvim-lsp.enable = true;
    plugins.nvim-cmp = {
      enable = true;
      sources = [
        { name = "nvim_lsp"; }
	{ name = "luasnip"; }
	{ name = "path"; }
	{ name = "buffer"; }
      ];
      snippet.expand = "luasnip";
      mapping = {
        "<CR>" = "cmp.mapping.confirm({ select = true })";
        "<Tab>" = {
          action = ''
            function(fallback)
	      local luasnip = require('luasnip')
              if cmp.visible() then
                cmp.select_next_item()
              elseif luasnip.expandable() then
                luasnip.expand()
              elseif luasnip.expand_or_jumpable() then
                luasnip.expand_or_jump()
              else
                fallback()
              end
            end
          '';
          modes = [ "i" "s" ];
        };
      };
    };

    plugins.cmp_luasnip.enable = true;
    plugins.luasnip = {
      enable = true;
      fromVscode = [
        {}
      ];
    };

    plugins.treesitter = {
      enable = true;
    };

    extraPlugins = with pkgs; [
      local.vimPlugins.lualine-lsp-progress
    ];

  };
}
