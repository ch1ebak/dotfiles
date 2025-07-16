# ============================================================
# ▗▄▄▄▖▗▄▄▄▖▗▄▄▄▖▗▖   ▗▄▄▄▖     ▗▄▄▖ ▗▄▖ ▗▖  ▗▖▗▄▄▄▖▗▄▄▄▖ ▗▄▄▖
# ▐▌ ▐▌  █    █  ▐▌   ▐▌       ▐▌   ▐▌ ▐▌▐▛▚▖▐▌▐▌     █  ▐▌
# ▐▌ ▐▌  █    █  ▐▌   ▐▛▀▀▘    ▐▌   ▐▌ ▐▌▐▌ ▝▜▌▐▛▀▀▘  █  ▐▌▝▜▌
# ▐▙▄▟▙▖ █  ▗▄█▄▖▐▙▄▄▖▐▙▄▄▖    ▝▚▄▄▖▝▚▄▞▘▐▌  ▐▌▐▌   ▗▄█▄▖▝▚▄▞▘
#
# github.com/ch1ebak/dotfiles
# ============================================================


# IMPORTS
import os
import subprocess
import re
import socket
from libqtile import bar, extension, hook, layout, qtile, widget
from libqtile.config import Click, Group, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile.widget import spacer
from typing import List
from qtile_extras import widget
from qtile_extras.widget.decorations import RectDecoration
from qtile_extras.widget.decorations import PowerLineDecoration

## Color scheme
from themes.spacegray import colors

# SETTINGS
## General
auto_fullscreen = True
auto_minimize = True
bring_front_click = False
cursor_warp = False
dgroups_key_binder = None
dgroups_app_rules = []
focus_on_window_activation = "smart"
follow_mouse_focus = False
reconfigure_screens = True
wmname = "LG3D"

### Floating
floats_kept_above = True,
floating_layout = layout.Floating(
    border_width = 2,
    border_focus = colors[5],
    border_normal = colors[2],
    float_rules=[
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),
        Match(wm_class="makebranch"),
        Match(wm_class="maketag"),
        Match(wm_class="ssh-askpass"),
        Match(title="branchdialog"),
        Match(wm_class="pinentry-gtk"),
        Match(wm_class="file-roller")
    ]
)

## Autostart
@hook.subscribe.startup_once
def start_once():
    qtile.cmd_spawn("xrandr --output HDMI-0 --primary --mode 1920x1080 --pos 1920x0 --rate 144 --output DP-0 --mode 1920x1080 --pos 0x0 --rate 165")
    qtile.cmd_spawn("nm-applet &")
    qtile.cmd_spawn("picom -b")
    qtile.cmd_spawn("dunst &")
    qtile.cmd_spawn("brightnessctl set 50%")
    qtile.cmd_spawn("/usr/bin/emacs --daemon &")
    for p in processes:
        subprocess.Popen(p)

## Groups
groups = [
    Group("1", label="󰭹", layout='max', matches=[Match(wm_class=["ferdium"])]),
    Group("2", label="󰈹", layout='max'),
    Group("3", label="󰈚", layout='max', matches=[Match(wm_class=["emacs"])]),
    Group("4", label="󰅴", layout='monadtall', matches=[Match(wm_class=["ghostty"])]),
    Group("5", label="󰝰", layout='monadtall', matches=[Match(wm_class=["Thunar"])]),
    Group("6", label="󰕊", layout='monadtall', matches=[Match(wm_class=["calibre", "qbittorrent", "virt-manager", "rawtherapee", "nwg-look", "nitrogen"])]),
    Group("7", label="󰓓", layout='max', matches=[Match(wm_class=["Steam", "steam", "lutris", "heroic"])]),
    Group("8", label="󰓇", layout='max', matches=[Match(wm_class=["spotify"])]),
    Group("9", label="󰕧", layout='max', matches=[Match(wm_class=["mpv"])])
]

## Layouts
layout_theme = {
    "border_width": 2,
    "margin": 8,
    "border_focus": colors[5],
    "border_normal": colors[2]
}

layouts = [
    layout.Max(**layout_theme),
    layout.MonadTall(**layout_theme),
    layout.MonadThreeCol(**layout_theme),
    layout.RatioTile(**layout_theme)
]

## Scripts
@lazy.function
def maximize_by_switching_layout(qtile):
    current_layout_name = qtile.current_group.layout.name
    if current_layout_name == 'monadtall':
        qtile.current_group.layout = 'max'
    elif current_layout_name == 'max':
        qtile.current_group.layout = 'monadtall'


# KEYS
## General
mod = "mod4"
prompt = "{0}@{1}: ".format(os.environ["USER"], socket.gethostname())

## Keybindings
keys = [
     # Apps
    Key([mod], "Return", lazy.spawn("ghostty")),
    Key([mod], "w", lazy.spawn("zen-browser")),
    Key([mod, "shift"], "w", lazy.spawn("zen-browser --private-window")),
    Key([mod], "e", lazy.spawn("emacsclient -c -a 'emacs'")),
    Key([mod], "a", lazy.spawn("ghostty -e yazi")),
    Key([mod, "shift"], "a", lazy.spawn("thunar")),

    # Menu
    Key([mod], "s", lazy.spawn("rofi -m 1 -show drun")),
    Key([mod], "u", lazy.spawn("/home/karna/.config/rofi/modules/rofi-utilities")),
    Key([mod], "p", lazy.spawn("slock")),
    Key([mod, "shift"], "p", lazy.spawn("rofi -m 1 -show power-menu -modi power-menu:~/.config/rofi/modules/rofi-power-menu")),

    # App control
    Key([mod], "q", lazy.window.kill()),
    Key([mod, "shift"], "q", lazy.spawn("xkill")),
    Key([mod, "control"], "r", lazy.restart()),
    Key([mod, "control"], "q", lazy.shutdown()),

    # Switch between screens (2 monitors)
    Key([mod], "x", lazy.to_screen(0)),
    Key([mod], "z", lazy.to_screen(1)),

    # Scroll through groups
    Key([mod], 'j', lazy.screen.prev_group(skip_managed=True, skip_empty=True)),
    Key([mod], 'k', lazy.screen.next_group(skip_managed=True, skip_empty=True)),

    # Move focus
    Key([mod], "h", lazy.layout.down()),
    Key([mod], "l", lazy.layout.next()),

    # Move windows
    Key([mod, "shift"], "h", lazy.layout.shuffle_left()),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right()),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up()),

    # Resize windows
    Key([mod], "n", lazy.layout.maximize()),
    Key([mod, "shift"], "n", lazy.layout.normalize()),
    Key([mod, "control"], "h", lazy.layout.grow_left()),
    Key([mod, "control"], "l", lazy.layout.grow_right()),
    Key([mod, "control"], "j", lazy.layout.shrink()),
    Key([mod, "control"], "k", lazy.layout.grow()),

    # Layouts
    Key(["shift", "control"], "Return", lazy.layout.toggle_split()),
    Key([mod, "shift"], "Tab", lazy.layout.flip()),
    Key([mod], "Tab", lazy.next_layout()),
    Key([mod], "Space", maximize_by_switching_layout()),
    Key([mod, "shift"], "Space", lazy.window.toggle_fullscreen())
]

## Move windows between groups
for i in groups:
    keys.extend(
        [
            Key(
                [mod],
                i.name,
                lazy.group[i.name].toscreen(),
                desc="Switch to group {}".format(i.name),
            ),
            Key(
                [mod, "shift"],
                i.name,
                lazy.window.togroup(i.name, switch_group=True),
                desc="Switch to & move focused window to group {}".format(i.name),
            ),
        ]
    )


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
wname = widget.WindowName(width=bar.CALCULATED, empty_group_string="Desktop", max_chars=130, foreground = colors[4], **dec1)
bat = widget.Battery(
            background = colors[1],
            foreground = colors[7],
            format = '{char} {percent:2.0%}',
            low_foreground = "#AA4A44",
            low_percentage = 0.2,
            notify_below = 0.2,
            charge_char = "",
            discharge_char = "",
            full_char = "",
            empty_char = "",
            not_charging_char = "",
            unknown_char_char = "?"
            )
date = widget.Clock(background = colors[2], foreground = colors[10], format = "  %a, %d.%m.%y - %H:%M")

## Screens
screens = [
    Screen(
        wallpaper = "~/.config/qtile/wallpapers/spacegray.png",
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
        wallpaper = "~/.config/qtile/wallpapers/spacegray.png",
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
