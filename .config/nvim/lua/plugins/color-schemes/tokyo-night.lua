return {
  'folke/tokyonight.nvim',
  priority = 1000,
  opts = {
    style = "night",
    transparent = true,
    terminal_colors = false,
    bg_float = "none",
    on_colors = function(c)
        c.bg_statusline = c.none
    end,
    on_highlights = function(hl, c)
      hl.TabLineFill = {
        bg = c.none,
      }
    end,
  }
}
