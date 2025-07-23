-- OPTIONS
local g = vim.g
local o = vim.o
local opt = vim.opt
local cmd = vim.cmd
local api = vim.api

-- Color Scheme
o.background = "dark"
cmd "colorscheme catppuccin"

-- Transparency
api.nvim_set_hl(0, "Normal", { bg = "none"})
api.nvim_set_hl(0, "NormalNC", { bg = "none"})
api.nvim_set_hl(0, "EndOfBuffer", { bg = "none"})

-- Tabline
-- opt.showtabline = 1
-- opt.tabline = ''

-- Transparent tabline appearance
-- cmd([[
--   hi TabLineFill guibg=NONE ctermfg=242 ctermbg=NONE
-- ]])
