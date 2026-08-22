return {
	"folke/trouble.nvim",
	opts = {}, -- for default options, refer to the configuration section for custom setup.
	cmd = "Trouble",
	keys = {
		{
			"<leader>tr",
			"<cmd>Trouble diagnostics toggle<cr>",
			desc = "[Trouble] Diagnostics",
		},
	},
}
