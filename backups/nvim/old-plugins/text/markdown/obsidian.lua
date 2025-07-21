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
    dir = "~/Dokumenty/notatki/",
    preferred_link_style = "markdown",
    disable_frontmatter = true,
  },
  keys = {
    { "<leader>nn", ":Obsidian quick_switch<cr>", desc = "Obsidian Notes" },
  },
  picker = {
    name = "snacks"
  },
  ui = { enable = false },
  daily_notes = {
    -- Optional, if you keep daily notes in a separate directory.
    folder = "notatki/03-codzienne",
    -- Optional, if you want to change the date format for the ID of daily notes.
    date_format = "%Y-%m-%d",
    -- Optional, if you want to change the date format of the default alias of daily notes.
    -- alias_format = "%B %-d, %Y",
    -- Optional, default tags to add to each new daily note created.
    default_tags = { "dziennik" },
    -- Optional, if you want to automatically insert a template from your template directory like 'daily.md'
    template = nil,
    -- Optional, if you want `Obsidian yesterday` to return the last work day or `Obsidian tomorrow` to return the next work day.
    workdays_only = true,
  },
}
