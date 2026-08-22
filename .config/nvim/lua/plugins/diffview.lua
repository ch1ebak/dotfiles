return {
	"sindrets/diffview.nvim",
	cond = has_git,
	cmd = { "DiffviewOpen", "DiffviewFileHistory" },
	keys = {
		{ "<leader>td", "<cmd>DiffviewOpen<CR>", desc = "[DiffView] Open" },
	},
}
