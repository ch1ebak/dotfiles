return {
  "nvim-telescope/telescope.nvim",
  branch = "0.1.x",
  dependencies = {
    "nvim-lua/plenary.nvim",
    { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
    'BurntSushi/ripgrep',
    "nvim-tree/nvim-web-devicons",
  },
  config = function()
    local telescope = require("telescope")
    local actions = require("telescope.actions")
    local transform_mod = require("telescope.actions.mt").transform_mod

    telescope.setup({
      defaults = {
        path_display = { "smart" },
        mappings = {
          i = {
            ["<C-k>"] = actions.move_selection_previous, -- move to prev result
            ["<C-j>"] = actions.move_selection_next, -- move to next result
          },
        },
        vimgrep_arguments = {
          'rg',
          '--color=never',
          '--no-heading',
          '--with-filename',
          '--line-number',
          '--column',
          '--smart-case'
        }
      },
      pickers = {
        find_files = {
          hidden = true
        }
      },
    })
    pcall(require('telescope').load_extension, 'fzf')
    local builtin = require 'telescope.builtin'
    vim.keymap.set('n', '<leader><space>', builtin.find_files, { desc = 'FZF' })
    vim.keymap.set('n', '<leader>,', builtin.buffers, { desc = 'Buffers' })
    vim.keymap.set('n', '<leader>/', builtin.current_buffer_fuzzy_find, { desc = 'Fuzzy find in current buffer' })
    vim.keymap.set('n', '<leader>?', builtin.live_grep, { desc = 'Grep' })
    vim.keymap.set('n', '<leader>fr', builtin.oldfiles, { desc = 'Search Recent Files' })
    vim.keymap.set('n', '<leader>hh', builtin.help_tags, { desc = 'Help Tags' })
    vim.keymap.set('n', '<leader>hk', builtin.keymaps, { desc = 'Keymaps' })
    vim.keymap.set('n', '<leader>ht', builtin.colorscheme, { desc = 'Change color scheme' })
    vim.keymap.set('n', '<leader>fp', function()
      builtin.find_files { cwd = '~/.dotfiles/.config/nvim/' }
    end, { desc = 'Search Neovim files' })
    vim.keymap.set('n', '<leader>fn', function()
      builtin.find_files { cwd = '~/Dokumenty/notatki/' }
    end, { desc = 'Search Notes' })
  end
}
