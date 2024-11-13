return {
  "folke/twilight.nvim",
  opts = {
  },
  config = function ()
    require("twilight").setup({
      vim.keymap.set("n", "<leader>tw", ":Twilight<CR>", { desc = 'Toggle Twilight' })
    })
  end
}
