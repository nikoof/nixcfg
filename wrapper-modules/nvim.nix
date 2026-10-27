inputs: {
  config,
  wlib,
  lib,
  pkgs,
  options,
  ...
}: {
  imports = [wlib.wrapperModules.neovim];

  options = {
    full = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable LSP and config for all defined specs";
    };
  };

  config = {
    package = pkgs.neovim-unwrapped;
    settings.config_directory = "/home/nikoof/.config/nvim";

    specMods = {config, ...}: {
      # See https://github.com/BirdeeHub/nix-wrapper-modules/discussions/506.
      # See https://github.com/nix-community/nix-wrapper-modules/blob/afb6c5e9c38d4cd244748d9a9f0a6c4d1d7cb0e2/templates/neovim/module.nix#L195
      options.runtimePkgs =
        options.runtimePkgs
        // {
          description = ''
            A runtimePkgs spec field to put packages on the PATH
            If the spec is disabled, this value will not be included in the resulting neovim derivation
          '';
        };
    };

    runtimePkgs = config.specCollect (acc: v: acc ++ (v.runtimePkgs or [])) [];

    info = {
      nixpkgs = "${inputs.nixpkgs}";
    };

    specs.general = {
      data = with pkgs.vimPlugins; [
        nvim-highlight-colors
        base16-nvim

        bufferline-nvim
        toggleterm-nvim
        smart-splits-nvim

        mini-align
        mini-pick
        mini-icons
        oil-nvim
        comment-nvim

        luasnip

        nvim-treesitter.withAllGrammars
      ];
    };

    specs.lsp = {
      enable = lib.mkDefault config.full;
      data = null;
      config =
        /*
        lua
        */
        ''
          vim.keymap.set("n", "<leader>lf", vim.lsp.buf.format)
          vim.keymap.set("n", "<leader>lr", vim.lsp.buf.rename)
          vim.keymap.set("n", "<leader>li", vim.lsp.buf.hover)
          vim.keymap.set("n", "<leader>ld", vim.diagnostic.open_float)
          vim.keymap.set("n", "gD", vim.lsp.buf.declaration, { noremap = true, silent = true })
          vim.keymap.set("n", "gd", vim.lsp.buf.definition, { noremap = true, silent = true })
          vim.keymap.set("n", "gr", vim.lsp.buf.references, { noremap = true, silent = true })
          vim.keymap.set("n", "gi", vim.lsp.buf.implementation, { noremap = true, silent = true })
          vim.keymap.set("n", "<leader>uh", function() vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled()) end)

          -- Use LSP completions
          vim.api.nvim_create_autocmd("LspAttach", {
            callback = function(ev)
              local client = vim.lsp.get_client_by_id(ev.data.client_id)
              if client == nil then
                return
              end

              if client:supports_method("textDocument/completion") then
                vim.bo[ev.buf].omnifunc = "v:lua.vim.lsp.omnifunc"
              end
            end,
          })
        '';
    };

    specs.nix = {
      enable = lib.mkDefault config.full;
      after = ["lsp"];
      runtimePkgs = [pkgs.nixd pkgs.alejandra];
      data = null;
      config =
        /*
        lua
        */
        ''
          vim.lsp.enable("nixd")
          vim.lsp.config("nixd", {
            cmd = { "nixd" },
            filetypes = { "nix" },
            root_markers = { "flake.nix", ".git" },
            settings = {
              nixd = {
                formatting = {
                  command = { "alejandra -qq" },
                },
                nixpkgs = {
                  expr = "import ${inputs.nixpkgs} { }",
                },
              },
            },
          })
        '';
    };

    specs.lua = {
      enable = lib.mkDefault config.full;
      after = ["lsp"];
      data = pkgs.vimPlugins.nvim-lspconfig;
      runtimePkgs = [pkgs.lua-language-server pkgs.stylua];
      config =
        /*
        lua
        */
        ''
          vim.lsp.enable("lua_ls")
          vim.lsp.config("lua_ls", {
            settings = {
              Lua = {
                workspace = {
                  library = vim.api.nvim_get_runtime_file("", true),
                },
              },
            },
          })
        '';
    };

    specs.lean = {
      enable = lib.mkDefault config.full;
      data = pkgs.vimPlugins.lean-nvim;
      config =
        /*
        lua
        */
        ''
          vim.keymap.set('n', '<leader>lv', '<Plug>(LeanInfoviewToggle)', { buffer = true })
          require("lean").setup({
            mappings = true,
            abbreviations = {
              enable = true,
              leader = '\\',
              extra = {
                wknight = '♘',
              },
            },

            lsp = {
              enhanced_handlers = {
                hover = true,
                diagnostics = true,
              },

              init_options = {
                editDelay = 10,
                hasWidgets = true,
              }
            },

            goal_markers = {
              unsolved = ' 🤔 ',
              accomplished = '∎',
            },

            graphics = {
              enabled = true,
            },
          })
        '';
    };

    specs.typst = {
      enable = lib.mkDefault config.full;
      after = ["lsp"];
      data = pkgs.vimPlugins.nvim-lspconfig;
      runtimePkgs = [pkgs.tinymist];
      config =
        /*
        lua
        */
        ''vim.lsp.enable("tinymist")'';
    };

    specs.c = {
      enable = lib.mkDefault config.full;
      after = ["lsp"];
      data = pkgs.vimPlugins.nvim-lspconfig;
      runtimePkgs = [pkgs.gdb pkgs.clang-tools];
      config =
        /*
        lua
        */
        ''
          vim.lsp.enable("clangd")
          vim.lsp.config("clangd", {
            filetypes = { 'c', 'cpp', 'h', 'hpp', 'chead' },
          })
        '';
    };

    specs.rust = {
      enable = lib.mkDefault config.full;
      after = ["lsp"];
      data = pkgs.vimPlugins.nvim-lspconfig;
      runtimePkgs = [pkgs.rust-analyzer pkgs.clippy];
      config =
        /*
        lua
        */
        ''vim.lsp.enable("rust_analyzer")'';
    };

    specs.haskell = {
      enable = lib.mkDefault config.full;
      after = ["lsp"];
      data = pkgs.vimPlugins.nvim-lspconfig;
      runtimePkgs = [pkgs.haskell-language-server];
      config =
        /*
        lua
        */
        ''vim.lsp.enable("haskell-language-server")'';
    };

    specs.python = {
      enable = lib.mkDefault config.full;
      after = ["lsp"];
      data = pkgs.vimPlugins.nvim-lspconfig;
      runtimePkgs = [pkgs.basedpyright];
      config =
        /*
        lua
        */
        ''vim.lsp.enable("basedpyright")'';
    };

    specs.java = {
      enable = lib.mkDefault config.full;
      after = ["lsp"];
      data = [pkgs.vimPlugins.nvim-lspconfig pkgs.vimPlugins.nvim-jdtls];
      runtimePkgs = [pkgs.jdt-language-server];
      config =
        /*
        lua
        */
        ''vim.lsp.enable("jdtls")'';
    };
  };
}
