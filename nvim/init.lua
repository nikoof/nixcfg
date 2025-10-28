-- globals
vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.opt.list = true
vim.opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }


vim.o.number = true
vim.o.relativenumber = true
vim.o.winborder = "double"
vim.o.signcolumn = "yes"

vim.o.tabstop = 2
vim.o.softtabstop = 2
vim.o.shiftwidth = 2
vim.o.expandtab = true

vim.o.undofile = true
vim.o.swapfile = false

vim.o.hlsearch = true
vim.o.incsearch = true
vim.o.ignorecase = true
vim.o.smartcase = true

vim.o.completeopt = "menu,preview,noselect"

vim.cmd("colorscheme base16-tomorrow-night")

-- packages
-- I want this to be mostly portable to non-Nix setups, so try to
-- install packages with the new nvim package manager.
local ok, _ = pcall(require, "nixCats")
if not ok then
  vim.pack.add({
    { src = "https://github.com/RRethy/base16-nvim" },
    { src = "https://github.com/akinsho/bufferline.nvim" },
    { src = "https://github.com/akinsho/toggleterm.nvim" },
    { src = "https://github.com/mrjones2014/smart-splits.nvim" },

    { src = "https://github.com/stevearc/oil.nvim" },
    { src = "https://github.com/nvim-mini/mini.pick" },
    { src = "https://github.com/nvim-mini/mini.align" },
    { src = "https://github.com/nvim-mini/mini.icons" },
  })
end

-- keybinds
local kmap = vim.keymap.set
kmap("n", "<leader>o", ":update<CR>:source<CR>")
kmap("n", "j", "gj")
kmap("n", "k", "gk")
kmap("n", "x", "\"_x")
kmap("n", "X", "\"_X")

-- system clipboard
kmap({ "n", "v", "x" }, "<leader>y", '"+y')
kmap({ "n", "v", "x" }, "<leader>d", '"+d')
kmap({ "n", "v", "x" }, "<leader>p", '"+p')

-- clear search on norm<esc>
kmap("n", "<esc>", ":nohlsearch<CR><esc>", { noremap = true, silent = true })

-- splits
local splits = require("smart-splits")
kmap("n", "\\", ":split<CR>")
kmap("n", "|", ":vsplit<CR>")
kmap("n", "<C-h>", splits.move_cursor_left)
kmap("n", "<C-j>", splits.move_cursor_down)
kmap("n", "<C-k>", splits.move_cursor_up)
kmap("n", "<C-l>", splits.move_cursor_right)
kmap("n", "<A-h>", splits.resize_left)
kmap("n", "<A-j>", splits.resize_down)
kmap("n", "<A-k>", splits.resize_up)
kmap("n", "<A-l>", splits.resize_right)

-- wtf? (toggleterm)
kmap("n", "<leader>tv", [[<cmd>exe v:count1 . "ToggleTerm direction=vertical"<CR>]])
kmap("n", "<leader>th", [[<cmd>exe v:count1 . "ToggleTerm direction=horizontal"<CR>]])
kmap("t", "<esc>", "<C-\\><C-n>")
kmap("t", "<C-j>", "<cmd>wincmd j<CR>")
kmap("t", "<C-k>", "<cmd>wincmd k<CR>")
kmap("t", "<C-l>", "<cmd>wincmd l<CR>")
kmap("t", "<C-h>", "<cmd>wincmd h<CR>")

-- quit
kmap("n", "<leader>c", ":bd<CR>", { noremap = true, silent = true })
kmap("n", "<leader>q", ":q<CR>", { noremap = true, silent = true })

-- filesystem binds
kmap("n", "<leader>e", ":Pick files<CR>")
kmap("n", "<leader><S-e>", ":Oil<CR>")

-- lsp binds
kmap("n", "<leader>lf", vim.lsp.buf.format)
kmap("n", "<leader>lr", vim.lsp.buf.rename)
kmap("n", "<leader>li", vim.lsp.buf.hover)
kmap("n", "<leader>ld", vim.diagnostic.open_float)
kmap("n", "gD", vim.lsp.buf.declaration, { noremap = true, silent = true })
kmap("n", "gd", vim.lsp.buf.definition, { noremap = true, silent = true })
kmap("n", "gr", vim.lsp.buf.references, { noremap = true, silent = true })
kmap("n", "gi", vim.lsp.buf.implementation, { noremap = true, silent = true })

-- UI binds
kmap("n", "<leader>uh", function() vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled()) end)

-- LSP
vim.lsp.enable({ "lua_ls", "nixd", "clangd", "rust_analyzer", "tinymist", "hls", "basedpyright" })
vim.lsp.config("lua_ls", {
    settings = {
        Lua = {
            workspace = {
                library = vim.api.nvim_get_runtime_file("", true),
            }
        }
    }
})

vim.lsp.config("nixd", {
    settings = {
        nixpkgs = {
            expr = nixCats.extra("nixdExtras.nixpkgs") or [[import <nixpkgs> {}]],
        },
        formatting = {
            -- XXX: it doesn't work
            command = { "alejandra" }
        },
    }
})

vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(ev)
        local client = vim.lsp.get_client_by_id(ev.data.client_id)
        if client ~= nil and client:supports_method("textDocument/completion") then
            vim.lsp.completion.enable(true, client.id, ev.buf, { autotrigger = false })
        end
    end
})

-- plugins
require("nvim-treesitter.configs").setup({ highlight = { enable = true, } })
require("bufferline").setup()
require("toggleterm").setup({
    size = function(term)
        if term.direction == "horizontal" then
            return 15
        elseif term.direction == "vertical" then
            return vim.o.columns * 0.4
        end
    end,
})
require("Comment").setup({
    toggler = {
        line = '<leader>/',
        block = '<leader>?',
    },
    opleader = {
        line = '<leader>/',
        block = '<leader>?',
    },
    mappings = {
        basic = true,
        extra = false,
    }
})


require("mini.pick").setup()
require("mini.align").setup()
require("oil").setup()

require("typst-preview").setup()
