return {
	"folke/snacks.nvim",
	priority = 1000,
	lazy = false,
	---@type snacks.Config
	opts = {
		bigfile = { enabled = true },
		explorer = { enabled = true },
		image = { enabled = true },
		notifier = { enabled = true },
		quickfile = { enabled = true },
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
				preset = "default",
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
		{
			"<leader><space>",
			function()
				Snacks.picker.smart()
			end,
			desc = "[Snacks] Find Files",
		},
		{
			"<leader><Return>",
			function()
				Snacks.picker.projects()
			end,
			desc = "[Snacks] Projects",
		},
		{
			"<leader>,",
			function()
				Snacks.picker.buffers()
			end,
			desc = "[Snacks] Buffers",
		},
		{
			"<leader>.",
			function()
				Snacks.explorer.open()
			end,
			desc = "[Snacks] Explorer",
		},
		-- Find
		{
			"<leader>fr",
			function()
				Snacks.picker.recent()
			end,
			desc = "[Snacks] Recent Files",
		},
		{
			"<leader>fp",
			function()
				Snacks.picker.files({ cwd = "~/.config/nvim/" })
			end,
			desc = "[Snacks] Neovim Config",
		},
		{
			"<leader>fn",
			function()
				Snacks.picker.files({ cwd = "~/Dokumenty/notatki/" })
			end,
			desc = "[Snacks] Notes",
		},
		-- Search
		{
			"<leader>/",
			function()
				Snacks.picker.lines()
			end,
			desc = "[Snacks] Buffer Lines",
		},
		{
			"<leader>?",
			function()
				Snacks.picker.grep()
			end,
			desc = "[Snacks] Grep",
		},
		-- Stuff
		{
			"<leader>hk",
			function()
				Snacks.picker.keymaps()
			end,
			desc = "[Snacks] Keymaps",
		},
		{
			"<leader>hh",
			function()
				Snacks.picker.help()
			end,
			desc = "[Snacks] Help Pages",
		},
		{
			"<leader>ht",
			function()
				Snacks.picker.colorschemes()
			end,
			desc = "[Snacks] Colorschemes",
		},
	},
}
