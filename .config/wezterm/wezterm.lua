local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config = {
  automatically_reload_config = true,
  enable_wayland = true,
  default_prog = { 'bash' },
  color_scheme = 'Tokyo Night',
  font = wezterm.font 'JetBrainsMono NF',
  font_size = 10.0,
  freetype_load_target = 'Light',
	initial_cols = 100,
	initial_rows = 30,
  adjust_window_size_when_changing_font_size = false,
  window_background_opacity = 1.0,
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
