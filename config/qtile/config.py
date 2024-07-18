#    ____  ______ ____ __     ______ #
#   / __ \/_  __//  _// /    / ____/ #
#  / / / / / /   / / / /    / __/    #
# / /_/ / / /  _/ / / /___ / /___    #
# \___\_\/_/  /___//_____//_____/    #

# IMPORTS
import os
import subprocess
import re
import socket
from libqtile import bar, extension, hook, layout, qtile, widget
from libqtile.config import Click, Drag, Group, Key, KeyChord, Match, Screen
from libqtile.lazy import lazy
from libqtile.widget import spacer
from typing import List

## Color scheme
from colors.spacegray import colors

# KEYBINDINGS
mod = "mod4"
prompt = "{0}@{1}: ".format(os.environ["USER"], socket.gethostname())

keys = [
     # Apps
    Key([mod], "Return", lazy.spawn("alacritty")),
    Key([mod, "shift"], "Return", lazy.spawn("alacritty -e bash")),

    Key([mod], "w", lazy.spawn("firefox")),
    Key([mod, "shift"], "w", lazy.spawn("/usr/bin/firefox --private-window")),

    Key([mod], "e", lazy.spawn("emacsclient -c -a 'emacs'")),

    Key([mod], "a", lazy.spawn("emacsclient -c -a 'emacs' --eval '(dired nil)'")),
    Key([mod, "shift"], "a", lazy.spawn("pcmanfm")),

    Key([mod], "s", lazy.spawn("rofi -m 1 -show drun")),

    Key([mod], "d", lazy.spawn("spotify")),

    Key([mod], "g", lazy.spawn("steam")),
    Key([mod, "shift"], "g", lazy.spawn("lutris")),

    # Power menu
    Key([mod], "p", lazy.spawn("slock")),
    Key([mod, "shift"], "p", lazy.spawn("rofi -m 1 -show power-menu -modi power-menu:~/.config/rofi/modules/rofi-power-menu")),

    # App control
    Key([mod], "q", lazy.window.kill()),
    Key([mod, "shift"], "q", lazy.spawn("xkill")),
    Key([mod, "control"], "r", lazy.restart()),
    Key([mod, "control"], "q", lazy.shutdown()),

    # Switch between screens (2 monitors)
    Key([mod], "z", lazy.to_screen(1)),
    Key([mod], "x", lazy.to_screen(0)),

    # Switch between windows
    Key([mod], "h", lazy.layout.down()),
    Key([mod], "l", lazy.layout.next()),

    # Switch between groups
    Key([mod], 'j', lazy.screen.prev_group(skip_managed=True, )),
    Key([mod], 'k', lazy.screen.next_group(skip_managed=True, )),

    # Move windows
    Key([mod, "shift"], "h", lazy.layout.shuffle_left()),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right()),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up()),

    # Grow windows.
    Key([mod, "control"], "n", lazy.layout.normalize()),
    Key([mod, "control"], "h", lazy.layout.grow_left()),
    Key([mod, "control"], "l", lazy.layout.grow_right()),
    Key([mod, "control"], "j", lazy.layout.shrink()),
    Key([mod, "control"], "k", lazy.layout.grow()),

    # Layouts
    Key(["shift", "control"], "Return", lazy.layout.toggle_split()),
    Key([mod, "shift"], "Tab", lazy.layout.flip()),
    Key([mod], "Tab", lazy.next_layout()),
    Key([mod], "Space", lazy.window.toggle_fullscreen())
]

# GROUPS
groups = [
       Group("1", label="", layout='monadtall'),
       Group("2", label="", layout='monadtall'),
       Group("3", label="", layout='monadtall', matches=[Match(wm_class=["emacs"])]),
       Group("4", label="", layout='monadtall', matches=[Match(wm_class=["alacritty", "Alacritty"])]),
       Group("5", label="", layout='monadtall', matches=[Match(wm_class=["pcmanfm"])]),
       Group("6", label="", layout='monadtall', matches=[Match(wm_class=["calibre", "kcc", "qbittorrent", "nitrogen", "openrgb", "lxappearance"])]),
       Group("7", label="", layout='max', matches=[Match(wm_class=["Steam", "steam", "lutris"])]),
       Group("8", label="", layout='max', matches=[Match(wm_class=["spotify"])]),
       Group("9", label="", layout='max', matches=[Match(wm_class=["mpv"])])
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

# LAYOUTS
layout_theme = {"border_width": 2,
                "margin": 6,
                "border_focus": colors[5],
                "border_normal": colors[2]
                }

layouts = [
    layout.Max(**layout_theme),
    layout.MonadWide(**layout_theme),
    layout.MonadTall(**layout_theme),
]

# WIDGETS
widget_defaults = dict(
    font='JetBrainsMono NF Bold Italic',
    fontsize=10,
    padding=5,
    foreground = colors[7],
    background = colors[0]
    )

# SCREENS
screens = [
 Screen(
         top=bar.Bar(
             [
             widget.Sep(
                     linewidth = 0,
                     padding = 5,
                     ),
             widget.CurrentLayoutIcon(
                     custom_icon_paths = [os.path.expanduser("~/.config/qtile/icons")],
                     padding = 5,
                     scale = 0.7
                     ),
             widget.TextBox(
                     text = "",
                     font='Font Awesome',
                     fontsize = 15,
                     foreground = colors[4],
                     ),
             widget.OpenWeather(
                     app_key = "1fcfd7f17c1c297646e7efb5bcfb2c8a",
                     cityid = "3088848",
                     format = '{main_temp}°',
                     metric = True
                     ),
             widget.TextBox(
                     text = "",
                     font='Font Awesome',
                     fontsize = 15,
                     foreground = colors[4],
                     ),
             widget.Clock(
                     format = "%a, %d.%m.%y",
                     foreground = colors[7],
                     ),
             widget.TextBox(
                     text = "",
                     font='Font Awesome',
                     fontsize = 15,
                     foreground = colors[4],
                     ),
             widget.Clock(
                     format = "%H:%M",
                     foreground = colors[7],
                     ),
             widget.TextBox(
                     text = "",
                     font='Font Awesome',
                     fontsize = 15,
                     foreground = colors[4],
                     ),
             widget.Spacer(
                     length = bar.STRETCH
                     ),
             widget.GroupBox(
                     disable_drag = True,
                     center_aligned = True,
                     font='Font Awesome',
                     fontsize = 13,
                     margin_y = 3,
                     margin_x = 0,
                     padding_y = 5,
                     padding_x = 3,
                     borderwidth = 3,
                     highlight_method = "line",
                     rounded = True,
                     inactive = colors[2],
                     active = colors[7],
                     highlight_color = colors[0],
                     this_current_screen_border = colors[6],
                     this_screen_border = colors[7],
                     other_current_screen_border = colors[1],
                     other_screen_border = colors[1],
                     foreground = colors[7],
                     background = colors[0]
                     ),
             widget.Spacer(
                     length = bar.STRETCH
                     ),
             widget.TextBox(
                     text = "",
                     font='Font Awesome',
                     fontsize = 15,
                     foreground = colors[4],
                     ),
             widget.Battery(
                     padding = 5,
                     format = '{char} {percent:2.0%}',
                     charge_char = "",
                     discharge_char = "",
                     full_char = "",
                     empty_char = "",
                     not_charging_char = "",
                     unknown_char_char = "?",
                     low_percentage = 0.2,
                     low_foreground = colors[6],
                     notify_below = 0.2
                     ),
             widget.TextBox(
                     text = "",
                     font='Font Awesome',
                     fontsize = 15,
                     foreground = colors[4],
                     ),
             widget.CPU(                                         
                     padding = 5,
                     format = '{load_percent}%',
                     ),
             widget.TextBox(
                     text = "",
                     font='Font Awesome',
                     fontsize = 15,
                     foreground = colors[4],
                     ),
             widget.Memory(
                     format = '{MemUsed: .0f}{mm}',
                     ),
             widget.TextBox(
                     text = "",
                     font='Font Awesome',
                     fontsize = 15,
                     foreground = colors[4],
                     ),
             widget.Systray(
                     icon_size = 19
                     ),
             widget.TextBox(
                     text = "",
                     font='Font Awesome',
                     fontsize = 15,
                     foreground = colors[4],
                     ),
             widget.Sep(
                     linewidth = 0,
                     padding = 5,
                     ),
             ],
             25,
             margin = [6, 6, 0, 6]
             ), ),
 Screen(
         top=bar.Bar(
             [
             widget.Sep(                                                                            
                     linewidth = 0,
                     padding = 5,
                     ),
             widget.CurrentLayoutIcon(
                     custom_icon_paths = [os.path.expanduser("~/.config/qtile/icons")],
                     padding = 5,
                     scale = 0.7
                     ),
             widget.TextBox(
                     text = "",
                     font='Font Awesome',
                     fontsize = 15,
                     foreground = colors[4],
                     ),
             widget.OpenWeather(
                     app_key = "1fcfd7f17c1c297646e7efb5bcfb2c8a",
                     cityid = "3088848",
                     format = '{main_temp}°',
                     metric = True
                     ),
             widget.TextBox(
                     text = "",
                     font='Font Awesome',
                     fontsize = 15,
                     foreground = colors[4],
                     ),
             widget.Clock(
                     format = "%a, %d.%m.%y",
                     foreground = colors[7],                                                        
                     ),
             widget.TextBox(
                     text = "",
                     font='Font Awesome',
                     fontsize = 15,
                     foreground = colors[4],
                     ),
             widget.Clock(
                     format = "%H:%M",
                     foreground = colors[7],
                     ),
             widget.TextBox(
                     text = "",
                     font='Font Awesome',
                     fontsize = 15,
                     foreground = colors[4],
                     ),
             widget.Spacer(
                     length = bar.STRETCH
                     ),
             widget.GroupBox(
                     disable_drag = True,
                     center_aligned = True,
                     font='Font Awesome',
                     fontsize = 13,
                     margin_y = 3,
                     margin_x = 0,
                     padding_y = 5,
                     padding_x = 3,
                     borderwidth = 3,
                     highlight_method = "line",
                     rounded = True,
                     inactive = colors[2],
                     active = colors[7],
                     highlight_color = colors[0],
                     this_current_screen_border = colors[6],
                     this_screen_border = colors[7],
                     other_current_screen_border = colors[1],
                     other_screen_border = colors[1],
                     foreground = colors[7],
                     background = colors[0]
                     ),
             widget.Spacer(
                     length = bar.STRETCH
                     ),
             widget.TextBox(
                     text = "",
                     font='Font Awesome',
                     fontsize = 15,
                     foreground = colors[4],
                     ),
             widget.Battery(
                     padding = 5,
                     format = '{char} {percent:2.0%}',
                     charge_char = "",
                     discharge_char = "",
                     full_char = "",
                     empty_char = "",
                     not_charging_char = "",
                     unknown_char_char = "?",
                     low_percentage = 0.2,
                     low_foreground = colors[6]
                     ),
             widget.TextBox(
                     text = "",
                     font='Font Awesome',
                     fontsize = 15,
                     foreground = colors[4],
                     ),
             widget.CPU(
                     padding = 5,
                     format = '{load_percent}%',
                     ),
             widget.TextBox(
                     text = "",
                     font='Font Awesome',
                     fontsize = 15,
                     foreground = colors[4],
                     ),
             widget.Memory(
                     format = '{MemUsed: .0f}{mm}',
                     ),
             widget.TextBox(
                     text = "",
                     font='Font Awesome',
                     fontsize = 15,
                     foreground = colors[4],
                     ),
             widget.Sep(
                     linewidth = 0,
                     padding = 5,
                     ),
             ],
             25,
             margin = [6, 6, 0, 6]
             ), ),
]

# SETTINGS
dgroups_key_binder = None
dgroups_app_rules = []  # type: List
follow_mouse_focus = False
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    *layout.Floating.default_float_rules,
    Match(wm_class='notification'),
    Match(wm_class='copyq')],
    border_width = 2,
    border_focus = colors[5],
    border_normal = colors[2]
    )
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True
auto_minimize = True

# AUTOSTART
@hook.subscribe.startup_once
def start_once():
    qtile.cmd_spawn("xrandr --output HDMI-0 --primary --mode 1920x1080 --pos 1920x0 --rotate normal --rate 144 --output DP-0 --mode 1920x1080 --pos 0x0 --rotate normal --rate 144")
    qtile.cmd_spawn("/usr/bin/emacs --daemon &")
    qtile.cmd_spawn("nm-applet &")
    qtile.cmd_spawn("nitrogen --restore &")
    qtile.cmd_spawn("picom -b")
    qtile.cmd_spawn("dunst &")
    qtile.cmd_spawn("batsignal -w 20 -c 15 -d 5 -p -f 90 -b")
    for p in processes:
        subprocess.Popen(p)

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
