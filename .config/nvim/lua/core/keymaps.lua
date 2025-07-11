-- Leader key
local g = vim.g
g.mapleader = " "
g.maplocalleader = " "

-- Keymaps
local keymap = vim.keymap
local opts = { noremap = true, silent = true }

-- Config
keymap.set('n', '<leader>hr', ":luafile %<cr>", { desc = 'Reload config' }, opts)

-- Windows/Splits/Buffers
keymap.set('n', 'j', "gj", { desc = 'Move by line' }, opts)
keymap.set('n', 'k', "gk", { desc = 'Move by line' }, opts)
keymap.set('n', '<C-q>', ":q<CR>", { desc = 'Close' }, opts)
keymap.set('n', '<C-m>', "<C-w>v", { desc = 'Split window horizontally' }, opts)
keymap.set('n', '<C-h>', "<C-w>h", { desc = 'Move to left window' }, opts)
keymap.set('n', '<C-l>', "<C-w>l", { desc = 'Move to right window' }, opts)
keymap.set('n', '<C-n>', "<cmd>tabnew<CR>", { desc = 'New tab' }, opts)
keymap.set('n', '<C-j>', "<cmd>tabn<CR>", { desc = 'Next tab' }, opts)
keymap.set('n', '<C-k>', "<cmd>tabp<CR>", { desc = 'Previous tab' }, opts)

-- Movement
keymap.set({ "n", "v" }, "gh", "^", { desc = "Go to the beginning line" })
keymap.set({ "n", "v" }, "gl", "$", { desc = "Go to the end of the line" })
keymap.set("v", "gl", "$h", { desc = "Go to the end of the line" })
keymap.set("n", "Y", "y$", { desc = "Yank to end of line" })
keymap.set("v", "J", ":m '>+1<CR>gv=gv", { desc = 'Move selected line down' }, opts)
keymap.set("v", "K", ":m '<-2<CR>gv=gv", { desc = 'Move selected line up' }, opts)
keymap.set("n", "J", "mzJ`z", { desc = 'Combine line with the one below' }, opts)
keymap.set("n", "n", "nzzzv", { desc = 'Better search next' }, opts)
keymap.set("n", "N", "Nzzzv", { desc = 'Better search previous' }, opts)
keymap.set('n', 'yc', 'yy<cmd>normal gcc<CR>p')

-- Toggles
keymap.set("n", "<leader>tx", "<cmd>!chmod +x %<CR>", { desc = 'Chmod open file' }, opts)
keymap.set("n", "<leader>tl", ":set wrap!<CR>", { desc = 'Line wrapping' }, opts)

-- Spelling
keymap.set("n", "<leader>ze", function()
  vim.opt.spelllang = "en"
  vim.cmd("echo 'Spell language set to English'")
end, { desc = "Spelling language English" })

keymap.set("n", "<leader>zp", function()
  vim.opt.spelllang = "pl"
  vim.cmd("echo 'Spell language set to Polish'")
end, { desc = "Spelling language Polish" })

keymap.set("n", "<leader>zb", function()
  vim.opt.spelllang = "en,pl"
  vim.cmd("echo 'Spell language set to English and Polish'")
end, { desc = "[P]Spelling language English and Polish" })

keymap.set("n", "<leader>zc", function()
  vim.cmd("normal! 1z=")
end, { desc = "Spelling suggestions" })

keymap.set("n", "<leader>za", function()
  vim.cmd("normal! zg")
end, { desc = "Spelling add word to spellfile" })
