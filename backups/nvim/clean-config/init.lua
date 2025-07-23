--  ▗▄▄▖▗▖   ▗▄▄▄▖ ▗▄▖ ▗▖  ▗▖    ▗▖  ▗▖▗▄▄▄▖ ▗▄▖ ▗▖  ▗▖▗▄▄▄▖▗▖  ▗▖
-- ▐▌   ▐▌   ▐▌   ▐▌ ▐▌▐▛▚▖▐▌    ▐▛▚▖▐▌▐▌   ▐▌ ▐▌▐▌  ▐▌  █  ▐▛▚▞▜▌
-- ▐▌   ▐▌   ▐▛▀▀▘▐▛▀▜▌▐▌ ▝▜▌    ▐▌ ▝▜▌▐▛▀▀▘▐▌ ▐▌▐▌  ▐▌  █  ▐▌  ▐▌
-- ▝▚▄▄▖▐▙▄▄▖▐▙▄▄▖▐▌ ▐▌▐▌  ▐▌    ▐▌  ▐▌▐▙▄▄▖▝▚▄▞▘ ▝▚▞▘ ▗▄█▄▖▐▌  ▐▌
--
-- github.com/ch1ebak

-- KEYMAPS
-- Leader key
local o = vim.o
local opt = vim.opt
local cmd = vim.cmd
local api = vim.api
local g = vim.g
g.mapleader = " "
g.maplocalleader = " "

-- Keymaps
local keymap = vim.keymap
local opts = { noremap = true, silent = true }

-- Config
keymap.set("n", "<leader>hr", ":luafile %<cr>", { desc = "Reload config" }, opts)
keymap.set("n", "<leader>fp", ":e ~/.dotfiles/backups/nvim/clean-config/init.lua<cr>", { desc = "Edit config" }, opts)

-- Files
keymap.set("n", "<leader>.", ":Explore<cr>", { desc = "Edit config" }, opts)
keymap.set("n", "<leader>nn", ":Explore ~/Dokumenty/notatki<cr>", { desc = "Notes folder" }, opts)
keymap.set("n", "<leader>,", ":e ", { desc = "Edit" }, opts)

-- Windows/Splits/Buffers
keymap.set("n", "<C-q>", ":q<CR>", { desc = "Close" }, opts)
keymap.set("n", "<C-n>", "<C-w>v", { desc = "Split window horizontally" }, opts)
keymap.set("n", "<C-h>", "<C-w>h", { desc = "Move to left window" }, opts)
keymap.set("n", "<C-l>", "<C-w>l", { desc = "Move to right window" }, opts)
keymap.set("n", "<C-t>", "<cmd>tabnew<CR>", { desc = "New tab" }, opts)
keymap.set("n", "<C-j>", "<cmd>tabn<CR>", { desc = "Next tab" }, opts)
keymap.set("n", "<C-k>", "<cmd>tabp<CR>", { desc = "Previous tab" }, opts)

-- Movement
keymap.set("n", "j", "gj", { desc = "Move by line" }, opts)
keymap.set("n", "k", "gk", { desc = "Move by line" }, opts)
keymap.set({ "n", "v" }, "gh", "^", { desc = "Go to the beginning line" })
keymap.set({ "n", "v" }, "gl", "$", { desc = "Go to the end of the line" })
keymap.set("v", "gl", "$h", { desc = "Go to the end of the line" })
keymap.set("n", "<C-d>", "<C-d>zz", { desc = "Half page down (centered)" }, opts)
keymap.set("n", "<C-u>", "<C-u>zz", { desc = "Half page up(centered)" }, opts)

-- Yanking
keymap.set("n", "Y", "y$", { desc = "Yank to end of line" })
keymap.set("n", "J", "mzJ`z", { desc = "Combine line with the one below" }, opts)
keymap.set("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move selected line down" }, opts)
keymap.set("v", "K", ":m '<-2<CR>gv=gv", { desc = "Move selected line up" }, opts)

-- Search
keymap.set("n", "n", "nzzzv", { desc = "Better search next" }, opts)
keymap.set("n", "N", "Nzzzv", { desc = "Better search previous" }, opts)

-- Other
keymap.set("n", "yc", "yy<cmd>normal gcc<CR>p", { desc = "Uncomment and Copy" }, opts)
keymap.set("i", "jk", "<ESC>", { desc = "Exit insert mode" }, opts)

-- Toggles
keymap.set("n", "<leader>tx", "<cmd>!chmod +x %<CR>", { desc = "Chmod open file" }, opts)
keymap.set("n", "<leader>tl", ":set wrap!<CR>", { desc = "Line wrapping" }, opts)

-- Spelling
keymap.set("n", "<leader>z,", function()
  vim.opt.spell = true
  vim.cmd("echo 'Spellcheck enabled'")
end, { desc = "Enable Spellcheck" })

keymap.set("n", "<leader>z.", function()
  vim.opt.spell = false
  vim.cmd("echo 'Spellcheck disabled'")
end, { desc = "Disable Spellcheck" })

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

-- Copy Full File-Path
vim.keymap.set("n", "<leader>yc", function()
	local path = vim.fn.expand("%:p")
	vim.fn.setreg("+", path)
	print("file:", path)
end)

-- Autoclose
keymap.set("i", "'", "''<left>")
keymap.set("i", "\"", "\"\"<left>")
keymap.set("i", "(", "()<left>")
keymap.set("i", "[", "[]<left>")
keymap.set("i", "{", "{}<left>")


-- OPTIONS
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
g.editorconfig = false
o.wildmenu = true
opt.wildoptions = "fuzzy"
opt.wildmode = "longest:full,full"

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
opt.path:append("**")                          -- include subdirectories in search

-- Splits
opt.splitright = true
opt.splitbelow = true
opt.inccommand = "split"
o.linebreak = true
opt.breakindent = true
opt.list = true
opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }

-- Treesitter
cmd("syntax off")
vim.api.nvim_create_autocmd("FileType", {
    callback = function(ev)
        pcall(vim.treesitter.start, ev.buf)
    end
})

-- Spelling
-- opt.spelllang = "en,pl"                        -- Spellcheck languages
-- opt.spell = true                               -- Enable spellcheck

-- Netrw
g.netrw_banner = 0                             -- Disable the banner
-- g.netrw_altv = 0                               -- change from left splitting to right splitting
g.netrw_liststyle = 3                          -- tree style view in netrw

-- Highlight when yanking
api.nvim_create_autocmd("TextYankPost", {
  desc = "Highlight when yanking (copying) text",
  group = api.nvim_create_augroup("kickstart-highlight-yank", { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})

-- UI
-- Color Scheme
o.background = "dark"
cmd "colorscheme default"

-- Transparency
api.nvim_set_hl(0, "Normal", { bg = "none"})
api.nvim_set_hl(0, "NormalNC", { bg = "none"})
api.nvim_set_hl(0, "EndOfBuffer", { bg = "none"})

-- Tabline
vim.opt.showtabline = 1  -- Always show tabline (0=never, 1=when multiple tabs, 2=always)
vim.opt.tabline = ''     -- Use default tabline (empty string uses built-in)

-- Transparent tabline appearance
cmd([[
  hi TabLineFill guibg=NONE ctermfg=242 ctermbg=NONE
  hi TabLineSel guifg=#1c1d23 guibg=#aaedb7
  hi TabLine guibg=#1c1d23 guifg=#c4c6cd
]])

--statusline
cmd "highlight StatusBG guibg=#1c1d23 guifg=#c4c6cd"
cmd "highlight StatusLineExtra guifg=#1c1d23 guibg=#aaedb7"
cmd "highlight StatusLineAccent guifg=#1c1d23 guibg=#ffc3fa"
cmd "highlight StatuslineAccent guifg=#1c1d23 guibg=#aaedb7"
cmd "highlight StatuslineInsertAccent guifg=#1c1d23 guibg=#f4d88c"
cmd "highlight StatuslineVisualAccent guifg=#1c1d23 guibg=#ffbcb5"
cmd "highlight StatuslineReplaceAccent guifg=#1c1d23 guibg=#83efef"
cmd "highlight StatuslineCmdLineAccent guifg=#1c1d23 guibg=#9fd8ff"
cmd "highlight StatuslineTerminalAccent guifg=#1c1d23 guibg=#c4c6cd"

local modes = {
  ["n"] = "NORMAL",
  ["no"] = "NORMAL",
  ["v"] = "VISUAL",
  ["V"] = "VISUAL LINE",
  [""] = "VISUAL BLOCK",
  ["s"] = "SELECT",
  ["S"] = "SELECT LINE",
  [""] = "SELECT BLOCK",
  ["i"] = "INSERT",
  ["ic"] = "INSERT",
  ["R"] = "REPLACE",
  ["Rv"] = "VISUAL REPLACE",
  ["c"] = "COMMAND",
  ["cv"] = "VIM EX",
  ["ce"] = "EX",
  ["r"] = "PROMPT",
  ["rm"] = "MOAR",
  ["r?"] = "CONFIRM",
  ["!"] = "SHELL",
  ["t"] = "TERMINAL",
}

local function mode()
  local current_mode = vim.api.nvim_get_mode().mode
  return string.format(" %s ", modes[current_mode]):upper()
end

local function update_mode_colors()
  local current_mode = vim.api.nvim_get_mode().mode
  local mode_color = "%#StatusLineAccent#"
  if current_mode == "n" then
      mode_color = "%#StatuslineAccent#"
  elseif current_mode == "i" or current_mode == "ic" then
      mode_color = "%#StatuslineInsertAccent#"
  elseif current_mode == "v" or current_mode == "V" or current_mode == "" then
      mode_color = "%#StatuslineVisualAccent#"
  elseif current_mode == "R" then
      mode_color = "%#StatuslineReplaceAccent#"
  elseif current_mode == "c" then
      mode_color = "%#StatuslineCmdLineAccent#"
  elseif current_mode == "t" then
      mode_color = "%#StatuslineTerminalAccent#"
  end
  return mode_color
end

local function filepath()
  local fpath = vim.fn.fnamemodify(vim.fn.expand "%", ":~:.:h")
  if fpath == "" or fpath == "." then
      return " "
  end

  return string.format(" %%<%s/", fpath)
end

local function filename()
  local fname = vim.fn.expand "%:t"
  if fname == "" then
      return ""
  end
  return fname .. " "
end

local function filetype()
  local filetype = vim.opt.filetype:get()

  local map = {
    lua = " 󰢱 LUA",
    help = " 󰋗 HELP",
    python = " 󰌠 PYTHON",
    netrw = "  NETRW",
    rust = " 󱘗 RUST",
    php = " 󰌟 PHP",
    fish = " 󰈺 FISH",
    sh = "  SH",
    zig = "  ZIG",
    text = " 󰀬 TEXT",
    markdown = "  MARKDOWN",
    typescript = " 󰛦 TYPESCRIPT",
    typescriptreact = " 󰛦 TYPESCRIPT (react)",
    javascript = " 󰌞 JAVASCRIPT",
    javascriptreact = " 󰌞 JAVASCRIPT (react)"
  }

  local result = map[filetype] or string.upper(filetype)

  return result
end

local function lineinfo()
  if vim.bo.filetype == "alpha" then
    return ""
  end
  return " %l:%c "
end

Statusline = {}

Statusline.active = function()
  return table.concat {
    "%#Statusline#",
    update_mode_colors(),
    mode(),
    "%#StatusBG# ",
    filepath(),
    filename(),
    "%=%#StatusLineExtra#",
    filetype(),
    lineinfo(),
  }
end

function Statusline.inactive()
  return table.concat {
    "%#StatusBG# ",
    filepath(),
    filename(),
  }
end

function Statusline.short()
  return "%#StatusLineNC#   NvimTree"
end

api.nvim_exec([[
  augroup Statusline
  au!
  au WinEnter,BufEnter * setlocal statusline=%!v:lua.Statusline.active()
  au WinLeave,BufLeave * setlocal statusline=%!v:lua.Statusline.inactive()
  au WinEnter,BufEnter,FileType NvimTree setlocal statusline=%!v:lua.Statusline.short()
  augroup END
]], false)
