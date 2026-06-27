-- OPTIONS
local o = vim.o
local cmd = vim.cmd

-- Color Scheme
vim.o.background = "dark"
vim.cmd 'colorscheme gruvbox'

-- Transparency
vim.api.nvim_set_hl(0, "Normal", { bg = "none"})
vim.api.nvim_set_hl(0, "NormalNC", { bg = "none"})
vim.api.nvim_set_hl(0, "EndOfBuffer", { bg = "none"})
