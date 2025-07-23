return {
  'echasnovski/mini.nvim',
  version = "*",
  {
    "echasnovski/mini.tabline",
    lazy = false,
    version = "*",
    config = function()
      require("mini.tabline").setup()
    end
  },
  {
    "echasnovski/mini.icons",
    lazy = false,
    version = "*",
    config = function()
      require("mini.icons").setup()
    end
  }
}
