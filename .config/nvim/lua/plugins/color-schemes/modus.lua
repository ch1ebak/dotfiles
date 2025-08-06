return {
  "miikanissi/modus-themes.nvim",
  priority = 1000,
  config = function ()
    require("modus-themes").setup({
      style = "auto",
      variant = "default", -- Theme comes in four variants `default`, `tinted`, `deuteranopia`, and `tritanopia`
      transparent = false, -- Transparent background (as supported by the terminal)
      dim_inactive = false, -- "non-current" windows are dimmed
      hide_inactive_statusline = true, -- Hide statuslines on inactive windows. Works with the standard **StatusLine**, **LuaLine** and **mini.statusline**
      line_nr_column_background = true, -- Distinct background colors in line number column. `false` will disable background color and fallback to Normal background
      sign_column_background = true, -- Distinct background colors in sign column. `false` will disable background color and fallback to Normal background
      styles = {
        -- Style to be applied to different syntax groups
        -- Value is any valid attr-list value for `:help nvim_set_hl`
        comments = { italic = true },
        keywords = { italic = true },
        functions = {},
        variables = {},
      },

      --- You can override specific color groups to use other groups or a hex color
      --- Function will be called with a ColorScheme table
      --- Refer to `extras/lua/modus_operandi.lua` or `extras/lua/modus_vivendi.lua` for the ColorScheme table
      ---@param colors ColorScheme
      on_colors = function(colors) end,

      --- You can override specific highlights to use other groups or a hex color
      --- Function will be called with a Highlights and ColorScheme table
      --- Refer to `extras/lua/modus_operandi.lua` or `extras/lua/modus_vivendi.lua` for the Highlights and ColorScheme table
      ---@param highlights Highlights
      ---@param colors ColorScheme
      on_highlights = function(highlights, colors) end,
    })
  end
}
