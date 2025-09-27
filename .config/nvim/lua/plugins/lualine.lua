return {
  'nvim-lualine/lualine.nvim',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  config = function()
    local function getWords()
      return tostring(vim.fn.wordcount().words)
    end
    require('lualine').setup {
      options = {
        icons_enabled = true,
        theme = 'dracula',
        section_separators = { left = '', right = ''},
        disabled_filetypes = {
          statusline = {},
          winbar = {},
        },
        ignore_focus = {},
        always_divide_middle = true,
        globalstatus = false,
        refresh = {
          statusline = 1000,
          tabline = 1000,
          winbar = 1000,
        }
      },
      sections = {
        lualine_a = { {'mode', separator = { left = '' }, right_padding = 2 } },
        lualine_b = {'diff'},
        lualine_c = {
          {
            'filename',
            file_status = true,
            path = 3,
          },
        },
        lualine_x = {'filetype'},
        lualine_y = { { getWords } },
        lualine_z = { {'progress', separator = { right = '' }, left_padding = 2 } },
      },
      inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = {'filename'},
        lualine_x = {'location'},
        lualine_y = {},
        lualine_z = {}
      },
      tabline = {},
      winbar = {},
      inactive_winbar = {},
      extensions = {}
    }
  end,
}
