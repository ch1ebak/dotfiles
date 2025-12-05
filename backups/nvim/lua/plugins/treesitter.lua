return {
  "nvim-treesitter/nvim-treesitter",
  event = { "BufReadPre", "BufNewFile" },
  build = ":TSUpdate",
  dependencies = {
    "windwp/nvim-ts-autotag",
  },
  config = function()
    -- import nvim-treesitter plugin
    local treesitter = require("nvim-treesitter.configs")

    -- configure treesitter
    treesitter.setup({ -- enable syntax highlighting
      highlight = {
        enable = true,
      },
      -- enable indentation
      indent = { enable = true },
      -- enable autotagging (w/ nvim-ts-autotag plugin)
      autotag = {
        enable = true,
      },
      -- ensure these language parsers are installed
      ensure_installed = {
        "vim",
        "regex",
        "lua",
        "bash",
        "markdown",
        "markdown_inline",
        "python",
        "toml",
        "json",
        "html",
        "css",
        "hyprlang",
      },
      incremental_selection = {
        enable = true,
        keymaps = {
          init_selection = "<C-space>",
          node_incremental = "<C-space>",
          scope_incremental = false,
          node_decremental = "<bs>",
        },
      },
    })
  vim.filetype.add {
    extension = { rasi = 'rasi' },
    pattern = {
      ['.*/waybar/config'] = 'jsonc',
      ['.*/hypr/.*%.conf'] = 'hyprlang',
    },
  }
  end,
}
