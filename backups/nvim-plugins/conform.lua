return {
  'stevearc/conform.nvim',
  opts = {},
  config = function()
  require("conform").setup({
    formatters_by_ft = {
       lua = { "stylua" },
       python = { "isort", "black" },
       toml = { "prettier" },
       bash = { "shellharden" },
    },
  })
  end
}
