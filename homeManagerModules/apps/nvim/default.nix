{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  cfg = config.apps.nvim;
in {
  imports = [
    inputs.nixvim.homeManagerModules.nixvim
  ];

  options = {
    apps.nvim.enable = lib.mkEnableOption "Enable nvim";
    apps.nvim.nixvim.enable = lib.mkEnableOption "Enable nixvim config";
  };

  config = lib.mkIf cfg.enable {
    home.sessionVariables = {
      EDITOR = "nvim";
    };

    home.packages = with pkgs; [
      neovide
    ];

    programs.nixvim = lib.mkIf cfg.nixvim.enable {
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

      maps = {
        normal = {
          # General
          "<leader>w" = {
            action = "<cmd>w<cr>";
            silent = true;
          };
          "<leader>q" = {
            action = "<cmd>confirm q<cr>";
            silent = false;
          };
          "<leader>n" = {
            action = "<cmd>enew<cr>";
            silent = true;
          };
          "<C-s>" = {
            action = "<cmd>w!<cr>";
            silent = true;
          };
          "<C-q>" = {
            action = "<cmd>qa!<cr>";
            silent = true;
          };
          "|" = {
            action = "<cmd>vsplit<cr>";
            silent = true;
          };
          "\\" = {
            action = "<cmd>split<cr>";
            silent = true;
          };

          # Tabs
          "]t" = {
            action = "vim.cmd.tabnext";
            silent = true;
          };
          "[t" = {
            action = "vim.cmd.tabprevious";
            silent = true;
          };

          # Windows
          "<leader>e" = {
            action = "<cmd>Neotree toggle<cr>";
            silent = true;
          };
          "<leader>o" = {
            action = ''
              function()
                if vim.bo.filetype == "neo-tree" then
                  vim.cmd.wincmd "p"
                else
                  vim.cmd.Neotree "focus"
                end
              end
            '';
            silent = true;
          };
        };
      };

      colorschemes.nord.enable = true;

      plugins.nix.enable = true;
      plugins.nvim-autopairs.enable = true;

      plugins.comment-nvim = {
        enable = true;
      };

      plugins.lualine = {
        enable = true;
        iconsEnabled = true;
        theme = "nord";

        sections.lualine_c = ["lsp_progress"];

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
          {name = "nvim_lsp";}
          {name = "luasnip";}
          {name = "path";}
          {name = "buffer";}
        ];

        snippet.expand = "luasnip";

        mapping = {
          "<C-n>" = "cmp.mapping.select_next_item()";
          "<C-p>" = "cmp.mapping.select_prev_item()";
          "<C-d>" = "cmp.mapping.scroll_docs(-4)";
          "<C-f>" = "cmp.mapping.scroll_docs(4)";
          "<CR>" = ''
            cmp.mapping.confirm {
              behavior = cmp.ConfirmBehavior.Replace,
              select = true
            }
          '';

          "<Tab>" = {
            action = ''
              function(fallback)
                local luasnip = require('luasnip')
                if cmp.visible() then
                  cmp.select_next_item()
                elseif luasnip.expand_or_locally_jumpable() then
                  luasnip.expand_or_jump()
                else
                  fallback()
                end
              end
            '';
            modes = ["i" "s"];
          };

          "<S-Tab>" = {
            action = ''
              function(fallback)
                local luasnip = require('luasnip')
                if cmp.visible() then
                  cmp.select_prev_item()
                elseif luasnip.locally_jumpable(-1) then
                  luasnip.jump(-1)
                else
                  fallback()
                end
              end
            '';
            modes = ["i" "s"];
          };
        };
      };

      plugins.telescope = {
        enable = true;
        highlightTheme = "nord";

        keymaps = {
          "gr" = "lsp_references";
          "<leader>s" = "lsp_document_symbols";
          "<leader>S" = "lsp_dynamic_workspace_symbols";
        };

        extensions.fzf-native = {
          enable = true;
          caseMode = "smart_case";
          fuzzy = true;
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

      plugins.neo-tree = {
        enable = true;
        useDefaultMappings = true;
      };

      extraPlugins = with pkgs; [
        local.vimPlugins.lualine-lsp-progress
      ];
    };
  };
}
