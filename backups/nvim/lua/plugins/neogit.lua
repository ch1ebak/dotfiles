return {
  "NeogitOrg/neogit",
  lazy = true,
  dependencies = {
    "sindrets/diffview.nvim",        -- optional
    "m00qek/baleia.nvim",            -- optional
    "folke/snacks.nvim",             -- optional
  },
  cmd = "Neogit",
  keys = {
    { "<leader>pm", "<cmd>Neogit<cr>", desc = "Show Neogit UI" }
  }
}
