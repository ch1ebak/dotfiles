return {
  "slugbyte/lackluster.nvim",
  lazy = false,
  priority = 1000,
  init = function()
    -- vim.cmd.colorscheme("lackluster")
    vim.cmd.colorscheme("lackluster-hack") -- my favorite
    -- vim.cmd.colorscheme("lackluster-mint")
  end,
  config = function()
    local lackluster = require("lackluster")
    lackluster.setup({
      tweak_syntax = {
          comment = lackluster.color.gray4, -- or gray5
      },
      tweak_background = {
          normal = 'none',
          telescope = 'none',
          menu = lackluster.color.gray3,
          popup = 'default',
      },
    })
  end,
}
