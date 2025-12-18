# IMPORTS
from libqtile import bar, widget
from libqtile.config import Screen
from libqtile.widget import spacer
from qtile_extras import widget

colors = [
    ["#1B1E28"],  # 0
    ["#262934"],  # 1
    ["#303340"],  # 2
    ["#889096"],  # 3
    ["#E4F0FB"],  # 4
    ["#5DE4C7"],  # 5
    ["#D0679D"]   # 6
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
            foreground = colors[5],
            format = '{char} {percent:2.0%}',
            low_foreground = colors[6],
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
        wallpaper = "~/.config/qtile/wallpapers/poimandres.png",
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
        wallpaper = "~/.config/qtile/wallpapers/poimandres.png",
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
