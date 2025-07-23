-- OPTIONS
local g = vim.g
local o = vim.o
local opt = vim.opt
local cmd = vim.cmd
local api = vim.api

-- Basic settings
api.nvim_command('filetype plugin indent on')
o.fileencoding = "utf-8"

-- Editor settings
o.number = true                                -- Line numbers
o.relativenumber = true                        -- Relative line numbers
o.cursorline = true                            -- Highlight current line
o.wrap = true                                  -- Line wrapping
o.scrolloff = 4                                -- Keep n lines above/below cursor
o.sidescrolloff = 4                            -- Keep n columns left/right of cursor

-- Indentation
o.tabstop = 2                                  -- Tab width
o.softtabstop = 2                              -- Indent width
o.shiftwidth = 2                               -- Soft tab stop
o.expandtab = true                             -- Spaces instead of tabs
o.smartindent = true                           -- Smart auto-indenting
o.autoindent = true                            -- Copy indent from current line

-- Search settings
o.ignorecase = true                            -- Case insensitive search
o.smartcase = true                             -- Case sensitive if uppercase in search
o.hlsearch = false                             -- Search results highlighting
o.incsearch = true                             -- Show matches as you type

-- Visual settings
opt.termguicolors = true                       -- Enable 24-bit colors
opt.showmode = false                           -- Mode in command line
opt.conceallevel = 2                           -- Don't hide markup 
opt.concealcursor = "nc"                       -- Don't hide cursor line markup 
vim.opt.guicursor = "n-v-c-sm:block,i-ci-ve:ver25,r-cr-o:hor20,a:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor"

-- File handling
opt.backup = false                             -- Creating backup files
opt.swapfile = false                           -- Creating swap files
opt.undofile = true                            -- Persistent undo
opt.undodir = vim.fn.expand("~/.config/nvim/undodir") -- Undo directory
opt.updatetime = 250                           -- Faster completion
opt.timeoutlen = 500                           -- Key timeout duration
opt.autoread = true                            -- Auto reload files changed outside vim
opt.autowrite = false                          -- Don't auto save

-- Behavior settings
o.hidden = true                                -- Hidden buffers
opt.errorbells = false                         -- No error bells
opt.backspace = "indent,eol,start"             -- Better backspace behavior
opt.mouse = "a"                                -- Mouse support
opt.clipboard:append("unnamedplus")            -- System clipboard

-- Splits
opt.splitright = true
opt.splitbelow = true
opt.inccommand = "split"
o.linebreak = true
opt.breakindent = true
opt.list = true
opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }

-- Spelling
opt.spelllang = "en,pl"                       -- Spellcheck languages
opt.spell = true                              -- Enable spellcheck

-- Highlight when yanking
api.nvim_create_autocmd("TextYankPost", {
  desc = "Highlight when yanking (copying) text",
  group = api.nvim_create_augroup("kickstart-highlight-yank", { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})

-- Netrw
g.netrw_banner = 0                             -- Disable the banner
g.netrw_altv = 0                               -- change from left splitting to right splitting
g.netrw_liststyle = 3                          -- tree style view in netrw
