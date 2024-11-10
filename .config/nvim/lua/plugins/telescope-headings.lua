return {
  "crispgm/telescope-heading.nvim",
  dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
  config = function()
    require("telescope").load_extension "heading"
      vim.keymap.set("n", "<leader>sl", ":Telescope heading<CR>", { desc = 'Telecope Headings' } )
  end
}
