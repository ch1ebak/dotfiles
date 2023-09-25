-------------------------------------
-------------------------------------
--     ____   ___    _____  ______ --
--    / __ ) /   |  / ___/ / ____/ --
--   / __  |/ /| |  \__ \ / __/    --
--  / /_/ // ___ | ___/ // /___    --
-- /_____//_/  |_|/____//_____/    --
-------------------------------------
-------------------------------------

-------------
-- OPTIONS -- 
-------------
vim.o.nocompatible = true

--misc
vim.o.autoread = true
vim.o.confirm = true
vim.o.hidden = true
vim.o.wildmenu = true
-- vim.o.noswapfile = true

--code
vim.o.nofoldenable = true

--indentation
vim.o.autoindent = true
vim.o.expandtab = true
vim.o.tabstop = 2

-- search
vim.o.hlsearch = false 
vim.o.smartcase = true
vim.o.ignorecase = true
vim.o.incsearch = true

-- text
vim.o.syntax = true 
--vim.o.encoding = UTF-8
vim.o.linebreak = true
vim.o.wrap = true
vim.o.spell = true
vim.o.spelllang = en_us, pl_pl

-- theme
vim.cmd.colorscheme 'spacemanspiff' 
vim.o.background = dark
vim.o.t_Co = 256
vim.o.notermguicolors = true
vim.g.rehash256 = 1

-- ui
vim.o.number = true
vim.o.relativenumber = true
vim.o.laststatus = 2         
vim.o.noshowmode = true
vim.o.ruler = true
vim.o.mouse = a
