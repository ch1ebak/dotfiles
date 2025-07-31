return {
  'echasnovski/mini.nvim',
  version = "*",
  {
    "echasnovski/mini.pairs",
    config = function()
      require("mini.pairs").setup()
    end
  },
  {
    "echasnovski/mini.surround",
    config = function()
      require("mini.surround").setup()
    end
  },
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
