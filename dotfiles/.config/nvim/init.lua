-- set leader key to space
vim.g.mapleader = ' '
vim.cmd("set nofoldenable")
vim.opt.termguicolors = true

-- show line number
vim.opt.number = true               -- show absolute number

-- set shell to use
vim.opt.shell = "/bin/zsh"

-- all 4 spaces
vim.opt.tabstop = 4                 -- number of visual spaces per TAB
vim.opt.softtabstop = 4             -- number of spaces in tab when editing
vim.opt.shiftwidth = 4              -- insert 4 spaces on a tab
vim.opt.expandtab = true            -- tabs are spaces

-- shortcuts
vim.api.nvim_set_keymap('n', '<C-s>', ":w <CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-q>', ":q <CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<CR>', ":noh <CR><CR>", { noremap = true, silent = true })

-- gui setup
vim.opt.guifont = "FiraCode Nerd Font:h14"

-- plugins
require("plugins")

-- status line
--require("statusline")

-- lsp
require("lsp")
vim.lsp.inlay_hint.enable() -- enable variable hint
