return {
  "Mofiqul/dracula.nvim",
  config = function()
    require("dracula").setup({
      show_end_of_buffer = false,
      transparent_bg = true,
      italic_comment = true,
    })
  end
}

