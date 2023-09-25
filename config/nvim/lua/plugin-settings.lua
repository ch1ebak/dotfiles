 -------------------------------------------------------------
 -------------------------------------------------------------
 --    _____  ______ ______ ______ ____ _   __ ______ _____ -- 
 --   / ___/ / ____//_  __//_  __//  _// | / // ____// ___/ -- 
 --   \__ \ / __/    / /    / /   / / /  |/ // / __  \__ \  -- 
 --  ___/ // /___   / /    / /  _/ / / /|  // /_/ / ___/ /  -- 
 -- /____//_____/  /_/    /_/  /___//_/ |_/ \____/ /____/   -- 
 -------------------------------------------------------------
 -------------------------------------------------------------
 

-- beacon
vim.g.beacon_enable = 1

-- harpoon
require("telescope").load_extension('harpoon')
require("harpoon").setup({ 
   global_settings = {
   -- sets the marks upon calling `toggle` on the ui, instead of require `:w`.
   save_on_toggle = true,

   -- saves the harpoon file upon every change. disabling is unrecommended.
   save_on_change = true,

   -- sets harpoon to run the command immediately as it's passed to the terminal when calling `sendCommand`.
   enter_on_sendcmd = false,

   -- closes any tmux windows harpoon that harpoon creates when you close Neovim.
   tmux_autoclose_windows = false,

   -- filetypes that you want to prevent from adding to the harpoon list menu.
   excluded_filetypes = { "harpoon" },

   -- set marks specific to each git branch inside git repository
   mark_branch = false,

   -- enable tabline with harpoon marks
   tabline = false,
   tabline_prefix = "   ",
   tabline_suffix = "   ",
   }
})
 
-- lualine
require('lualine').setup {
 options = {
   theme = 'nordic',
   component_separators = '|',
   section_separators = { left = '', right = '' },
 },
 sections = {
   lualine_a = {
     { 'mode', separator = { left = '' }, right_padding = 2 },
   },
   lualine_b = { 'filename', 'branch' },
   lualine_c = { 'fileformat' },
   lualine_x = {},
   lualine_y = { 'filetype', 'progress' },
   lualine_z = {
     { 'location', separator = { right = '' }, left_padding = 2 },
   },
 },
 inactive_sections = {
   lualine_a = { 'filename' },
   lualine_b = {},
   lualine_c = {},
   lualine_x = {},
   lualine_y = {},
   lualine_z = { 'location' },
 },
 tabline = {},
 extensions = {},
}

-- markdown
vim.g.markdown_fenced_languages = {'html', 'python', 'bash=sh'}
vim.g.markdown_syntax_conceal = 1
vim.g.markdown_minlines = 100
vim.g.vim_markdown_folding_disabled = 1

-- nvim treesitter
require'nvim-treesitter.configs'.setup {
  ensure_installed = { "python", "lua", "vim", "vimdoc", "query", "bash", "fish", "html", "yaml", "markdown_inline" },
  sync_install = false,
  auto_install = true,
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
  },
}

-- org-mode
require('orgmode').setup_ts_grammar()
require('nvim-treesitter.configs').setup {
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = {'org'},
  },
  ensure_installed = {'org'}, -- Or run :TSUpdate org
}
require('orgmode').setup({
  org_agenda_files = {'~/Dropbox/org/*', '~/my-orgs/**/*'},
  org_default_notes_file = '~/Dropbox/org/refile.org',
})

-- table mode
vim.g.table_mode_corner = '|'
