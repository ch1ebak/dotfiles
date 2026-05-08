return {
  'danilamihailov/beacon.nvim',
  opts = {
    enabled = true,
    speed = 2,
    width = 40,
    winblend = 50,
    fps = 60,
    min_jump = 10,
    cursor_events = { 'CursorMoved' },
    window_events = { 'WinEnter', 'FocusGained' },
    highlight = { bg = 'white', ctermbg = 15 },
  }
}
