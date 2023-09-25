local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require('lazy').setup({

  -- telescope
  {'nvim-telescope/telescope.nvim', tag = '0.1.2',
    depencencies = { {'nvim-lua/plenary.nvim'} }},

  'debugloop/telescope-undo.nvim',

  {"nvim-telescope/telescope-file-browser.nvim",
    depencencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" }},

  -- other
  'danilamihailov/beacon.nvim',

  'dhruvasagar/vim-table-mode',

  'godlygeek/tabular',

  'jiangmiao/auto-pairs',

  {'nvim-lualine/lualine.nvim',
    depencencies = { 'nvim-tree/nvim-web-devicons', opt = true }},

  {'nvim-treesitter/nvim-treesitter',
    build = function()
        local ts_update = require('nvim-treesitter.install').update({ with_sync = true })
        ts_update()
    end,},

  'nvim-tree/nvim-web-devicons',

  'preservim/vim-markdown',

  'ThePrimeagen/harpoon',

  'tpope/vim-commentary',

  'tpope/vim-eunuch',

  'vim-scripts/colorizer',

  'vimwiki/vimwiki',

  -- colors
  
  'AlexvZyl/nordic.nvim',

  {'csexton/spacemanspiff.vim', as = 'spacemanspiff'},

  'LuRsT/austere.vim',

  { "catppuccin/nvim", as = "catppuccin" },

})

