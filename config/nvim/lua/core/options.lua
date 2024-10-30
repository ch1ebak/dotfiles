local g = vim.g
local o = vim.o
local opt = vim.opt

vim.api.nvim_command('filetype plugin indent on')
o.fileencoding = 'utf-8'

opt.mouse = "a"
opt.showmode = false
o.number = true
o.relativenumber = true
o.wrap = true
o.linebreak = true
o.cursorline = true

o.scrolloff = 8
o.sidescrolloff = 8

opt.termguicolors = true
o.background = 'dark'

opt.breakindent = true
o.autoindent = true
o.smartindent = true
o.shiftwidth = 2
o.tabstop = 2
o.softtabstop = 2
o.expandtab = true

opt.conceallevel = 2
opt.concealcursor = 'nc'

o.hidden = true
o.ignorecase = true
o.smartcase = true
o.hlsearch = false
o.incsearch = true

opt.splitright = true
opt.splitbelow = true

opt.list = true
opt.listchars = { tab = '» ', trail = '·', nbsp = '␣' }

opt.inccommand = 'split'

opt.updatetime = 250
opt.timeoutlen = 500

opt.swapfile = false
opt.backup = false

opt.clipboard:append("unnamedplus")

vim.api.nvim_create_autocmd('TextYankPost', {
  desc = 'Highlight when yanking (copying) text',
  group = vim.api.nvim_create_augroup('kickstart-highlight-yank', { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})

