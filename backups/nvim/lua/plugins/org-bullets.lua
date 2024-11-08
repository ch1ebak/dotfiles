return {
  'nvim-orgmode/org-bullets.nvim',
  config = function()
    require("org-bullets").setup {
      concealcursor = false,
      symbols = {
        list = "•",
        headlines = { "◉", "○", "◉", "○" },
        checkboxes = {
          half = { "󰿟", "@org.checkbox.halfchecked" },
          done = { "󰄬", "@org.keyword.done" },
          todo = { "󰍴", "@org.keyword.todo" },
        },
      }
    }
  end
}
