return {
  'echasnovski/mini.nvim',
  version = "*",
  lazy = false,
  config = function ()
    require("mini.sessions").setup(
      {
        file = '',
      }
    )
    require("mini.pairs").setup()
    require("mini.surround").setup()
    require("mini.icons").setup()
    require("mini.tabline").setup()
    require("mini.files").setup()
    vim.keymap.set("n", "<leader>.", "<CMD>lua MiniFiles.open()<CR>", { desc = "Open parent directory in floating window" })
  end
}
