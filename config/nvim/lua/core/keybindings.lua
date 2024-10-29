-- Leader key
local g = vim.g
g.mapleader = " "
g.maplocalleader = " "

-- Keymaps
local keymap = vim.keymap
local opts = { noremap = true, silent = true }

keymap.set('n', '<leader><Tab><Return>', "<C-w>v", { desc = 'Split window horizontally' }, opts)
keymap.set('n', '<leader><Tab>h', "<C-w>h", { desc = 'Move to left window' }, opts)
keymap.set('n', '<leader><Tab>l', "<C-w>l", { desc = 'Move to right window' }, opts)
keymap.set('n', '<leader><Tab>n', "<cmd>tabnew<CR>", { desc = 'New tab' }, opts)
keymap.set('n', '<leader><Tab>j', "<cmd>tabn<CR>", { desc = 'Next tab' }, opts)
keymap.set('n', '<leader><Tab>k', "<cmd>tabp<CR>", { desc = 'Previous tab' }, opts)
keymap.set('n', '<leader><Tab>q', ":q<CR>", { desc = 'Close' }, opts)
keymap.set('n', '<leader><Tab>H', "<C-^>", { desc = 'Switch between buffers' }, opts)
keymap.set('n', '<leader><Tab>L', "<C-^>", { desc = 'Switch between buffers' }, opts)
keymap.set('n', '<leader><Tab>J', "<C-W>R", { desc = 'Move splits around' }, opts)
keymap.set('n', '<leader><Tab>L', "<C-W>R", { desc = 'Move splits around' }, opts)
keymap.set('n', '<leader>ww', ":e ~/Dokumenty/notatki/index.md<CR>", { desc = 'Close' }, opts)
