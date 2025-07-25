local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config = {
  automatically_reload_config = true,
  default_prog = { '/bin/bash' },
  color_scheme = 'Poimandres',
  font = wezterm.font 'JetBrainsMono NF',
  font_size = 10.0,
  freetype_load_target = 'Light',
  window_background_opacity = 0.9,
  text_background_opacity = 1.0,
  max_fps = 120,
  default_cursor_style = 'BlinkingBar',
  enable_tab_bar = false,
  window_close_confirmation = 'NeverPrompt',
  window_padding = {
    left = 6,
    right = 6,
    top = 6,
    bottom = 6,
  },
}

return config
