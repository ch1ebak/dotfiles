--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--     __ __  ________  __ ____   ____ _   __ ____   ____ _   __ ______ _____ -- 
--    / //_/ / ____/\ \/ // __ ) /  _// | / // __ \ /  _// | / // ____// ___/ -- 
--   / ,<   / __/    \  // __  | / / /  |/ // / / / / / /  |/ // / __  \__ \  -- 
--  / /| | / /___    / // /_/ /_/ / / /|  // /_/ /_/ / / /|  // /_/ / ___/ /  -- 
-- /_/ |_|/_____/   /_//_____//___//_/ |_//_____//___//_/ |_/ \____/ /____/   -- 
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

vim.g.mapleader = " "

-- misc
vim.api.nvim_set_keymap('n', '<leader><leader>', ':', { noremap = true })
vim.api.nvim_set_keymap('n', '<leader>hr', ':PackerSync<cr>', { noremap = true })
vim.api.nvim_set_keymap('n', '<leader>hc', ':source ~/.config/nvim/init.lua<cr>', { noremap = true })

-- beacon
vim.api.nvim_set_keymap('n', '<leader>=', ':Beacon<cr>', { noremap = true })

-- harpoon
vim.api.nvim_set_keymap('n', '<leader><Return>', ':Telescope harpoon marks<cr>', { noremap = true })

-- spelling
vim.api.nvim_set_keymap('n', '<leader>ap', ':setlocal spell spelllang=pl<cr>', { noremap = true })
vim.api.nvim_set_keymap('n', '<leader>ae', ':setlocal spell spelllang=en_us<cr>', { noremap = true })

-- splits
vim.api.nvim_set_keymap('n', '<leader>ph', ':split<cr>', { noremap = true })
vim.api.nvim_set_keymap('n', '<leader>pv', ':vsplit<cr>', { noremap = true })
vim.api.nvim_set_keymap('n', '<C-H>', '<C-W><C-H>', { noremap = true })
vim.api.nvim_set_keymap('n', '<C-J>', '<C-W><C-J>', { noremap = true })
vim.api.nvim_set_keymap('n', '<C-K>', '<C-W><C-K>', { noremap = true })
vim.api.nvim_set_keymap('n', '<C-L>', '<C-W><C-L>', { noremap = true })

-- table-mode
vim.api.nvim_set_keymap('n', '<leader><Tab>', ':TableModeEnable<cr>', { noremap = true })

-- telescope
local builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>,', builtin.buffers, {})
vim.keymap.set('n', '<leader>fr', builtin.oldfiles, {})
vim.keymap.set('n', '<leader>ht', builtin.colorscheme, {})
vim.keymap.set('n', '<leader>hh', builtin.help_tags, {})
vim.keymap.set('n', '<leader>sb', builtin.current_buffer_fuzzy_find, {})
-- vim.keymap.set('n', '<leader>sf', builtin.find_files, {})
vim.keymap.set('n', '<leader>sf', '<cmd>Telescope find_files hidden=true<cr>', {})
vim.keymap.set('n', '<leader>sr', builtin.live_grep, {})

-- telescope file browser
require("telescope").load_extension "file_browser"
vim.api.nvim_set_keymap('n', '<leader>.', ':Telescope file_browser<cr>', { noremap = true })

-- telescope-undo
vim.api.nvim_set_keymap('n', '<leader>u', ':lua require("telescope").extensions.undo.undo()<cr>', { noremap = true })
