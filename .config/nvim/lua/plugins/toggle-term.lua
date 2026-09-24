return {
  'akinsho/toggleterm.nvim',
  version = "*",
  opts = {
    direction = "float",
  },
	keys = {
		{
			"<leader>pt",
      ":ToggleTerm<CR>",
			desc = "[Conform] Format Buffer",
		},
	},
}
