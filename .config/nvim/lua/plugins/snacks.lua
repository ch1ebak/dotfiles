return {
  "folke/snacks.nvim",
  priority = 1000,
  lazy = false,
  opts = {
    animate = { enabled = true},
    bigfile = { enabled = true },
    explorer = { enabled = true },
    image = { enabled = true },
    indent = { enabled = true },
    notifier = { enabled = true },
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
    quickfile = { enabled = true },
    scroll = { enabled = true },
    statuscolumn = { enabled = true },
    words = { enabled = false },
  },
  keys = {
    -- Top Pickers & Explorer
    { "<leader><space>", function() Snacks.picker.smart() end, desc = "Smart Find Files" },
    { "<leader>,", function() Snacks.picker.buffers() end, desc = "Buffers" },
    { "<leader><", function() Snacks.picker.grep() end, desc = "Grep" },
    { "<leader>.", function() Snacks.explorer() end, desc = "File Explorer" },
    -- { "<leader>.", function() Snacks.picker.files() end, desc = "Find Files" },
    -- find
    { "<leader><return>", function() Snacks.picker.projects() end, desc = "Projects" },
    { "<leader>fp", function() Snacks.picker.files({ cwd = '~/.dotfiles/.config/nvim/' }) end, desc = "Find Config File" },
    { "<leader>fr", function() Snacks.picker.recent() end, desc = "Recent" },
    -- Grep
    { "<leader>sb", function() Snacks.picker.lines() end, desc = "Buffer Lines" },
    { "<leader>sB", function() Snacks.picker.grep_buffers() end, desc = "Grep Open Buffers" },
    -- search
    { '<leader>s/', function() Snacks.picker.search_history() end, desc = "Search History" },
    { "<leader>:", function() Snacks.picker.commands() end, desc = "Commands" },
    { "<leader>;", function() Snacks.picker.command_history() end, desc = "Command History" },
    { "<leader>?", function() Snacks.picker.help() end, desc = "Help Pages" },
    { "<leader>ti", function() Snacks.picker.icons() end, desc = "Icons" },
    { "<leader>/", function() Snacks.picker.keymaps() end, desc = "Keymaps" },
    { "<leader>tu", function() Snacks.picker.undo() end, desc = "Undo History" },
    { "<leader>ht", function() Snacks.picker.colorschemes() end, desc = "Colorschemes" },
  },
}
