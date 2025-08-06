return {
  'echasnovski/mini.nvim',
  version = "*",
  lazy = false,
  config = function ()
    require("mini.pairs").setup()
    require("mini.surround").setup()
    require("mini.icons").setup()
    require("mini.tabline").setup()
  end
}
