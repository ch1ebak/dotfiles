return {
  'nvimdev/dashboard-nvim',
  event = 'VimEnter',
  config = function()
    require('dashboard').setup {
      theme = 'hyper',
      disable_move = true,
      shortcut_type = 'letter',
      shuffle_letter = true,
      change_to_vcs_root = false,
      config = {
        week_header = {
         enable = true,
        },
        shortcut = {
          { desc = '󰊳  Update', group = '@property', action = 'Lazy update', key = 'h' },
          {
            desc = '  Files',
            group = 'Label',
            action = 'Telescope find_files',
            key = 'j',
          },
          {
            desc = ' Zoxide',
            group = 'Number',
            action = 'Telescope zoxide',
            key = 'l',
          },
          {
            desc = '  Notes',
            group = 'DiagnosticHint',
            action = ':lua require(\"kiwi\").open_wiki_index()',
            key = 'k',
          },
        },
      },
      hide = {
        statusline = true,
        tabline = true,
        winbar = true,
      },
    }
  end,
  dependencies = { {'nvim-tree/nvim-web-devicons'}}
}
