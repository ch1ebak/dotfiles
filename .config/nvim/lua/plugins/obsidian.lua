return {
  "obsidian-nvim/obsidian.nvim",
  version = "*", -- recommended, use latest release instead of latest commit
  lazy = true,
  ft = "markdown",
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
  ---@module 'obsidian'
  ---@type obsidian.config
  opts = {
    dir = "~/Dokumenty/notatki/"
  },
  keys = {
    { "<leader>nn", ":Obsidian quick_switch<cr>", desc = "Obsidian Notes" },
  },
  picker = {
    name = "picker"
  }
}
