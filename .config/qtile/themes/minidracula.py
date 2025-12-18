# IMPORTS
from libqtile import bar, widget
from libqtile.config import Screen
from libqtile.widget import spacer
from qtile_extras import widget

colors = [
    ["#20212B"],  # 0
    ["#282A36"],  # 1
    ["#3D3F4A"],  # 2
    ["#63656E"],  # 3
    ["#E6E6E6"],  # 4
    ["#5AF78E"],  # 5
    ["#FF6E67"]   # 6
]

# BAR
## Widgets
widget_defaults = dict(
    font='JetBrainsMono NF',
    fontsize=10,
    padding=5,
    foreground = colors[4],
    background = colors[0])

spacer1 = widget.Spacer()
sep = widget.Sep(foreground = colors[0], linewidth = 5)

groupbox = widget.GroupBox(
            disable_drag = True,
            center_aligned = True,
            hide_unused = True,
            rounded = True,
            highlight_method = "line",
            font="JetBrainsMono Nerd Font Mono",
            fontsize = 16,
            margin_y = 3,
            margin_x = 0,
            padding_y = 5,
            padding_x = 5,
            foreground = colors[3],
            active = colors[4],
            inactive = colors[3],
            highlight_color = colors[0],
            this_current_screen_border = colors[5],
            this_screen_border = colors[3],
            other_screen_border = colors[3],
            other_current_screen_border = colors[5]
            )
wname = widget.WindowName(width=bar.CALCULATED, empty_group_string="Desktop", max_chars=130)
bat = widget.Battery(
            foreground = colors[6],
            format = '{char} {percent:2.0%}',
            low_foreground = colors[5],
            low_percentage = 0.2,
            notify_below = 0.2,
            charge_char = "",
            discharge_char = "",
            full_char = "",
            empty_char = "",
            not_charging_char = "",
            unknown_char_char = "?"
            )
date = widget.Clock(format = "  %a, %d.%m.%y - %H:%M")

## Screens
screens = [
    Screen(
        wallpaper = "~/.config/qtile/wallpapers/dracula.png",
        wallpaper_mode = "fill",
        top=bar.Bar(
            [
            sep,
            groupbox,
            spacer1,
            wname,
            spacer1,
            bat,
            sep,
            date,
            sep
            ],
            26,
            margin = [0, 0, 0, 0]
        )
    ),
    Screen(
        wallpaper = "~/.config/qtile/wallpapers/dracula.png",
        wallpaper_mode = "fill",
        top=bar.Bar(
            [
            sep,
            groupbox,
            spacer1,
            wname,
            spacer1,
            bat,
            sep,
            date,
            sep
            ],
            26,
            margin = [0, 0, 0, 0]
        )
    )
]
