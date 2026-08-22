local g = vim.g
local g = vim.g
local keymap = vim.keymap
local opts = { noremap = true, silent = true }

-- Leader key
g.mapleader = " "
g.maplocalleader = " "

-- Config
keymap.set("n", "<leader>hr", ":restart<cr>", { desc = "Reload config" })

-- Sessions
keymap.set("n", "<leader>ss", ":mksession! ~/.local/share/nvim/session/", { desc = "Save session" })
keymap.set("n", "<leader>sl", ":source ~/.local/share/nvim/session/", { desc = "Load session" })

-- Windows/Splits/Buffers
keymap.set("n", "<C-w>", ":q<CR>", { desc = "Close" })
keymap.set("n", "<C-c>", ":bdelete<CR>", { desc = "Close" })
keymap.set("n", "<C-n>", "<C-w>v", { desc = "Split window horizontally" })
keymap.set("n", "<C-h>", "<C-w>h", { desc = "Move to left window" })
keymap.set("n", "<C-l>", "<C-w>l", { desc = "Move to right window" })
keymap.set("n", "<C-t>", "<cmd>tabnew<CR>", { desc = "New tab" })
keymap.set("n", "<C-k>", "<cmd>tabn<CR>", { desc = "Next tab" })
keymap.set("n", "<C-j>", "<cmd>tabp<CR>", { desc = "Previous tab" })
keymap.set("n", "<C-S-h>", "<C-^>", { desc = "Previous tab" })

-- Movement
keymap.set("n", "j", "gj", { desc = "Move by line - down" })
keymap.set("n", "k", "gk", { desc = "Move by line - up" })
keymap.set({ "n", "v" }, "gh", "^", { desc = "Go to the beginning line" })
keymap.set({ "n", "v" }, "gl", "$", { desc = "Go to the end of the line" })
keymap.set("v", "gl", "$h", { desc = "Go to the end of the line" })
keymap.set("n", "<C-d>", "<C-d>zz", { desc = "Half page down (centered)" })
keymap.set("n", "<C-u>", "<C-u>zz", { desc = "Half page up (centered)" })

-- Yanking
keymap.set("n", "Y", "y$", { desc = "Yank to end of line" })
keymap.set("n", "J", "mzJ`z", { desc = "Combine line with the one below" })
keymap.set("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move selected line down" })
keymap.set("v", "K", ":m '<-2<CR>gv=gv", { desc = "Move selected line up" })

-- Search
keymap.set("n", "n", "nzzzv", { desc = "Better search next" })
keymap.set("n", "N", "Nzzzv", { desc = "Better search previous" })

-- Other
keymap.set("i", "HL", "<ESC>", { desc = "Exit insert mode" })
keymap.set("n", "yc", "yy<cmd>normal gcc<CR>p", { desc = "Uncomment and Copy" })

local function duplicate_and_comment()
	local esc = vim.api.nvim_replace_termcodes("<Esc>", true, false, true)
	vim.api.nvim_feedkeys(esc, "x", false)
	local start_line = vim.fn.line("'<")
	local end_line = vim.fn.line("'>")
	vim.cmd(start_line .. "," .. end_line .. "yank")
	vim.cmd((end_line + 1) .. "put")
	vim.api.nvim_feedkeys("gv", "n", false)
	vim.api.nvim_feedkeys("gc", "v", false)
end

keymap.set("v", "yc", duplicate_and_comment, { noremap = true, desc = "Duplicate selection and comment original" })

-- Toggles
keymap.set("n", "<leader>tl", ":set wrap!<CR>", { desc = "Line wrapping" })

-- LSP
keymap.set("n", "grh", "<cmd>lua vim.diagnostic.open_float()<CR>", { desc = "Diagnostics - at point" })

-- Copy Full File-Path
vim.keymap.set("n", "<leader>yc", function()
	local path = vim.fn.expand("%:p")
	vim.fn.setreg("+", path)
	print("file:", path)
end)

-- Focus split
keymap.set("n", "<C-o>", function()
	local win = vim.api.nvim_get_current_win()
	local wwidth = vim.api.nvim_win_get_width(win)
	local wheight = vim.api.nvim_win_get_height(win)

	local tab_width = vim.o.columns
	local tab_height = vim.o.lines - vim.o.cmdheight

	local focused = wwidth >= tab_width * 0.9 and wheight >= tab_height * 0.9
	if focused then
		vim.cmd("wincmd =") --equalize all win size
	else
		vim.cmd("wincmd |")
		vim.cmd("wincmd _")
	end
end)
