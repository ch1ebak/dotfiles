return {
	"stevearc/conform.nvim",
	event = "BufWritePre",
	cmd = { "ConformInfo" },
	opts = {
		format_on_save = {
			timeout_ms = 500,
			lsp_format = "fallback",
		},
		formatters_by_ft = {
			bash = { "beautysh", "shellcheck", stop_after_first = true },
			c = { "astyle" },
			css = { "rustywind" },
			json = { "jq" },
			lua = { "stylua" },
			nix = { "nixfmt" },
			sh = { "shellcheck" },
		},
	},
	keys = {
		{
			"<leader>tf",
			function()
				require("conform").format({ async = true })
			end,
			desc = "[Conform] Format Buffer",
		},
	},
}
