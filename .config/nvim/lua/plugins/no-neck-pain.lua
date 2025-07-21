-- Creates evenly sized empty buffers on each side of your focused buffer, which acts as padding for your window.
return {
  "shortcuts/no-neck-pain.nvim",
  version = "*",
  keys = {
      { "<leader>tv", ":NoNeckPain<CR>", desc = "Center text" }
  },
  buffers = {
      blend = -0.2,
  },
}
