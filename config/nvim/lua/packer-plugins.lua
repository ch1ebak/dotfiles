--------------------------------------------------
--------------------------------------------------
--     ____   ___    ______ __ __  ______ ____  --
--    / __ \ /   |  / ____// //_/ / ____// __ \ --
--   / /_/ // /| | / /    / ,<   / __/  / /_/ / --
--  / ____// ___ |/ /___ / /| | / /___ / _, _/  --
-- /_/    /_/  |_|\____//_/ |_|/_____//_/ |_|   --
--------------------------------------------------
--------------------------------------------------

-- This file can be loaded by calling `lua require('plugins')` from your init.vim

-- Only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)

  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  -- telescope
  use {
    'nvim-telescope/telescope.nvim', tag = '0.1.2',
    requires = { {'nvim-lua/plenary.nvim'} }
  }
  use 'debugloop/telescope-undo.nvim'
  use {
    "nvim-telescope/telescope-file-browser.nvim",
    requires = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" }
  }

  -- other
  use 'danilamihailov/beacon.nvim'
  use 'dhruvasagar/vim-table-mode'
  use 'godlygeek/tabular'
  use 'jiangmiao/auto-pairs'
  use {
    'nvim-lualine/lualine.nvim',
    requires = { 'nvim-tree/nvim-web-devicons', opt = true }
  }
  use {
    'nvim-treesitter/nvim-treesitter',
    run = function()
        local ts_update = require('nvim-treesitter.install').update({ with_sync = true })
        ts_update()
    end,
    }
  use 'nvim-tree/nvim-web-devicons'
  use {'nvim-orgmode/orgmode', config = function()
    require('orgmode').setup{}
    end
    }
  use 'preservim/vim-markdown'
  use 'ThePrimeagen/harpoon'
  use 'tpope/vim-commentary'
  use 'tpope/vim-eunuch'
  use 'vim-scripts/colorizer'

  -- colors
  use 'AlexvZyl/nordic.nvim'
  use {'csexton/spacemanspiff.vim', as = 'spacemanspiff'}
  use 'LuRsT/austere.vim'
  use { "catppuccin/nvim", as = "catppuccin" }

end)

