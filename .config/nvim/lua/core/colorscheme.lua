-- OPTIONS
local g = vim.g
local o = vim.o
local opt = vim.opt
local cmd = vim.cmd
local api = vim.api

-- Color Scheme
o.background = "dark"
cmd 'colorscheme catppuccin'

-- Transparency
-- api.nvim_set_hl(0, "Normal", { bg = "none"})
-- api.nvim_set_hl(0, "NormalNC", { bg = "none"})
-- api.nvim_set_hl(0, "EndOfBuffer", { bg = "none"})

-- Tabline
-- vim.opt.showtabline = 1  -- Always show tabline (0=never, 1=when multiple tabs, 2=always)
-- vim.opt.tabline = ''     -- Use default tabline (empty string uses built-in)

-- Transparent tabline appearance
-- cmd([[
--   hi TabLineFill guibg=NONE ctermfg=242 ctermbg=NONE
--   hi TabLineSel guifg=#1B1E28 guibg=#5DE4C7
--   hi TabLine guibg=#1B1E28 guifg=#E4F0FB
-- ]])
