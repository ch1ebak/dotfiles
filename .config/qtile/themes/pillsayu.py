# IMPORTS
from libqtile import bar, widget
from libqtile.config import Screen
from libqtile.widget import spacer
from qtile_extras import widget
from qtile_extras.widget.decorations import RectDecoration
from qtile_extras.widget.decorations import PowerLineDecoration

colors = [
    ["#1F2430"],  # 0
    ["#242B38"],  # 1
    ["#343F4C"],  # 2
    ["#3E4B59"],  # 3
    ["#CCCAC2"],  # 4
    ["#FFD173"],  # 5
    ["#D5FF80"],  # 6
    ["#FF6666"]   # 7
]

# BAR
## Decorations
dec1 = {
    "decorations": [
        RectDecoration(colour = colors[2], radius = 5, filled = True, padding_y = 2, group = True)
    ],
    "padding": 20,
}

pleft = {
    "decorations": [
        PowerLineDecoration(path = "rounded_left")
    ]
}

sleft = {
    "decorations": [
        PowerLineDecoration(path = "forward_slash")
    ]
}

pright = {
    "decorations": [
        PowerLineDecoration(path = "rounded_right", override_colour = colors[0], override_next_colour = colors[1])
    ]
}

sright = {
    "decorations": [
        PowerLineDecoration(path = "back_slash", override_colour = colors[1], override_next_colour = colors[2])
    ]
}

## Widgets
widget_defaults = dict(
    font='JetBrainsMono NF',
    fontsize=10,
    padding=5,
    foreground = colors[4],
    background = colors[0])

spacer1 = widget.Spacer()
sep = widget.Sep(background = colors[2], foreground = colors[2], linewidth = 5)
roundl = widget.Spacer(length = 1, background = colors[1], **pleft)
slashl = widget.Spacer(length = 1, background = colors[2], **sleft)
roundr = widget.Spacer(length = 1, **pright)
slashr = widget.Spacer(length = 1, **sright)

layouticon = widget.CurrentLayoutIcon(background = colors[2], scale = 0.6)
systray = widget.Systray(background = colors[2],icon_size = 19)
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
            background = colors[1],
            foreground = colors[3],
            active = colors[4],
            inactive = colors[3],
            highlight_color = colors[1],
            this_current_screen_border = colors[5],
            this_screen_border = colors[3],
            other_screen_border = colors[3],
            other_current_screen_border = colors[5]
            )
wname = widget.WindowName(width=bar.CALCULATED, empty_group_string="Desktop", max_chars=130, **dec1)
bat = widget.Battery(
            background = colors[1],
            foreground = colors[6],
            format = '{char} {percent:2.0%}',
            low_foreground = colors[7],
            low_percentage = 0.2,
            notify_below = 0.2,
            charge_char = "",
            discharge_char = "",
            full_char = "",
            empty_char = "",
            not_charging_char = "",
            unknown_char_char = "?"
            )
date = widget.Clock(background = colors[2], format = "  %a, %d.%m.%y - %H:%M")

## Screens
screens = [
    Screen(
        wallpaper = "~/.config/qtile/wallpapers/ayu.png",
        wallpaper_mode = "fill",
        top=bar.Bar(
            [
            sep,
            layouticon,
            systray,
            slashl,
            groupbox,
            roundl,
            spacer1,
            wname,
            spacer1,
            roundr,
            bat,
            slashr,
            date,
            sep
            ],
            26,
            margin = [8, 60, 0, 60]
        )
    ),
    Screen(
        wallpaper = "~/.config/qtile/wallpapers/ayu.png",
        wallpaper_mode = "fill",
        top=bar.Bar(
            [
            sep,
            layouticon,
            slashl,
            groupbox,
            roundl,
            spacer1,
            wname,
            spacer1,
            roundr,
            bat,
            slashr,
            date,
            sep
            ],
            26,
            margin = [6, 60, 0, 60]
        )
    )
]
