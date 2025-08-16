return {
  "folke/snacks.nvim",
  priority = 1000,
  lazy = false,
  ---@type snacks.Config
  opts = {
    bigfile = { enabled = true },
    image = { enabled = true },
    quickfile = { enabled = true },
    notifier = { enabled = true },
    indent = {
      enabled = true,
      animate = { enabled = false },
    },
    picker = {
      sources = {
        files = { hidden = true },
        grep = { hidden = true },
        explorer = { hidden = true },
      },
      layout = {
        preset = "ivy",
        cycle = true,
      },
      matcher = {
        frecency = true,
      },
      win = {
        input = {
          keys = {
            ["<Esc>"] = { "close", mode = { "n", "i" } },
            ["q"] = "close",
            ["J"] = { "preview_scroll_down", mode = { "i", "n" } },
            ["K"] = { "preview_scroll_up", mode = { "i", "n" } },
            ["H"] = { "preview_scroll_left", mode = { "i", "n" } },
            ["L"] = { "preview_scroll_right", mode = { "i", "n" } },
          },
        },
      },
      formatters = {
        file = {
          filename_first = true, -- display filename before the file path
          truncate = 80,
        },
      },
    },
  },
  keys = {
    -- Top Pickers
    { "<leader><space>", function() Snacks.picker.smart() end, desc = "Smart Find Files" },
    { "<leader><Return>", function() Snacks.picker.projects() end, desc = "Projects" },
    { "<leader>,", function() Snacks.picker.buffers() end, desc = "Buffers" },
    { "<leader>.", function() Snacks.picker.files() end, desc = "Find Files" },
    -- Find
    { "<leader>fr", function() Snacks.picker.recent() end, desc = "Recent" },
    { "<leader>fp", function() Snacks.picker.files({ cwd = '~/.dotfiles/.config/nvim/' }) end, desc = "Config File" },
    { "<leader>fn", function() Snacks.picker.files({ cwd = '/nvme/Dokumenty/notatki/' }) end, desc = "Config File" },
    -- Search
    { "<leader>/", function() Snacks.picker.lines() end, desc = "Buffer Lines" },
    { "<leader>?", function() Snacks.picker.grep() end, desc = "Grep" },
    -- Stuff
    { "<leader>ti", function() Snacks.picker.icons() end, desc = "Icons" },
    { "<leader>tu", function() Snacks.picker.undo() end, desc = "Undo History" },
    { "<leader>hk", function() Snacks.picker.keymaps() end, desc = "Keymaps" },
    { "<leader>hh", function() Snacks.picker.help() end, desc = "Help Pages" },
    { "<leader>ht", function() Snacks.picker.colorschemes() end, desc = "Colorschemes" },
  },
}
