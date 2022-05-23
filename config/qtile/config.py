import os
import re
import socket
import subprocess
from libqtile import qtile
from libqtile.config import Click, Drag, Group, KeyChord, Key, Match, Screen
from libqtile.command import lazy
from libqtile import layout, bar, widget, hook, extension
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal
from typing import List  # noqa: F401from typing import List  # noqa: F401
from libqtile.widget import spacer

mod = "mod4"
# terminal = guess_terminal("kitty")
terminal = guess_terminal("alacritty")
prompt = "{0}@{1}: ".format(os.environ["USER"], socket.gethostname())

keys = [
     # Apps
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),

    Key([mod], "a", lazy.spawn("emacsclient -c -a 'emacs' --eval '(dired nil)'"), desc="Launch Dired"),
    Key([mod, "shift"], "a", lazy.spawn("pcmanfm"), desc="Launch pcmanfm"),

    Key([mod], "d", lazy.spawn("signal-desktop"), desc="Launch signal"),

    Key([mod], "e", lazy.spawn("emacsclient -c -a 'emacs'"), desc='Launch Emacsclient'),
    Key([mod, "shift"], "e", lazy.spawn("emacs"), desc='Launch emacs'),

    Key([mod], "m", lazy.spawn("rhythmbox"), desc="Launch rhythmbox"),

    Key([mod], "p", lazy.spawn("slock"), desc="Launch lock screen"),
    Key([mod, "shift"], "p", lazy.spawn("rofi -show power-menu -modi power-menu:~/.config/rofi/modules/rofi-power-menu"), desc="Launch Rofi Power Menu"),

    Key([mod], "r", lazy.spawn("keepassxc"), desc="Launch keepassxc"),

    Key([mod], "s", lazy.spawn("rofi -show drun"), desc="Launch rofi"),

    Key([mod], "w", lazy.spawn("firefox-nightly"), desc="Launch firefox"),
    Key([mod, "shift"], "w", lazy.spawn("firefox-esr"), desc="Launch Firefox ESR"),

    # App control
    Key([mod], "q", lazy.window.kill(), desc="Kill focused window"),
    Key([mod, "control"], "r", lazy.restart(), desc="Restart Qtile"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),

    # Switch between groups
    Key([], 'XF86Back', lazy.screen.prev_group(skip_managed=True, )),
    Key([], 'XF86Forward', lazy.screen.next_group(skip_managed=True, )),
    Key([mod], 'XF86Back', lazy.screen.prev_group(skip_managed=True, )),
    Key([mod], 'XF86Forward', lazy.screen.next_group(skip_managed=True, )),
    Key([mod], 'Left', lazy.screen.prev_group(skip_managed=True, )),
    Key([mod], 'Right', lazy.screen.next_group(skip_managed=True, )),
    Key([mod], 'Escape', lazy.screen.togglegroup()),

    # Switch between screens
    Key([mod], "z", lazy.to_screen(2)),
    Key([mod], "c", lazy.to_screen(1)),
    Key([mod], "x", lazy.to_screen(0)),

    # Switch between windows
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "space", lazy.layout.next(),
        desc="Move window focus to other window"),

    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(),
        desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(),
        desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(),
        desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),

    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    Key([mod, "control"], "h", lazy.layout.grow_left(),
        desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(),
        desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.shrink(),
        desc="Shrink window"),
    Key([mod, "control"], "k", lazy.layout.grow(), desc="Grow window"),

    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key([mod, "control"], "Return", lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack"),

    # Toggle between different layouts as defined below
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
]

# groups = [
       # Group("1", label="", layout='monadwide', matches=[Match(wm_class=["signal", "discord", "teams"])]),
       # Group("2", label="", layout='monadtall'),
       # Group("3", label="", layout='monadtall'),
       # Group("4", label="", layout='monadtall'),
       # Group("5", label="", layout='monadtall', matches=[Match(wm_class=["calibre", "catfish", "shotwell", "czkawka", "szyszka"])]),
       # Group("6", label="", layout='monadtall', matches=[Match(wm_class=["steam", "lutris", "heroic", "airshipper", "veloren"])]),
       # Group("7", label="", layout='monadtall', matches=[Match(wm_class=["virt-manager","virtualbox", "gimp"])]),
       # Group("8", label="", layout='monadtall', matches=[Match(wm_class=["deadbeef", "spotify"])]),
       # Group("9", label="", layout='monadtall', matches=[Match(wm_class=["vlc", "mpv"])])
       # ]

groups = [
       Group("1", label="", layout='monadwide', matches=[Match(wm_class=["signal", "discord", "teams"])]),
       Group("2", label="", layout='monadtall'),
       Group("3", label="", layout='monadtall', matches=[Match(wm_class=["alacritty"])]),
       Group("4", label="", layout='monadtall'),
       Group("5", label="", layout='monadtall'),
       Group("6", label="", layout='monadtall'),
       Group("7", label="", layout='monadtall'),
       Group("8", label="", layout='monadtall', matches=[Match(wm_class=["spotify", "rhythmbox"])]),
       Group("9", label="", layout='monadtall', matches=[Match(wm_class=["vlc", "mpv"])])
       ]

for i in range(len(groups)):
    keys.append(Key([mod], str((i)), lazy.group[str(i)].toscreen()))
    keys.append(
        Key([mod, "shift"], str((i)), lazy.window.togroup(str(i), switch_group=True))
    )

# Catpuccin
#colors = [
 #["#1a1823", "#1a1823"],  # 0 background
 #["#6e6c7e", "#6e6c7e"],  # 1 foreground
 #["#302d42", "#302d42"],  # 2 background lighter
 #["#f28fad", "#f28fad"],  # 3 red
 #["#abe9b3", "#abe9b3"],  # 4 green
 #["#fae3b0", "#fae3b0"],  # 5 yellow
 #["#96cdfb", "#96cdfb"],  # 6 blue
 #["#e8a2af", "#e8a2af"],  # 7 maroon
 #["#89dceb", "#89dceb"],  # 8 cyan
 #["#c3bac6", "#c3bac6"],  # 9 grey
 #["#d9e0ee", "#d9e0ee"],  # 10 white
 #["#f8bd96", "#f8bd96"],  # 11 orange
 #["#8fbcbb", "#8fbcbb"],  # 12 super cyan
 #["#c9cbff", "#c9cbff"],  # 13 super blue
 #["#131020", "#131020"],  # 14 super dark background
 #["#988ba2", "#988ba2"]   # 15 slate grey
#]

## Dracula
#colors = [
# ["#282a36", "#282a36"],  # 0 background
# ["#f8f8f2", "#f8f8f2"],  # 1 foreground
# ["#6272a4", "#6272a4"],  # 2 background lighter
# ["#ff5555", "#ff5555"],  # 3 red
# ["#50fa7b", "#50fa7b"],  # 4 green
# ["#f1fa8c", "#f1fa8c"],  # 5 yellow
# ["#8be9fd", "#8be9fd"],  # 6 blue
# ["#ff79c6", "#ff79c6"],  # 7 magenta
# ["#8be9fd", "#8be9fd"],  # 8 cyan
# ["#44475a", "#44475a"],  # 9 grey
# ["#f8f8f2", "#f8f8f2"],  # 10 white
# ["#ffb86c", "#ffb86c"],  # 11 orange
# ["#8be9fd", "#8be9fd"],  # 12 super cyan
# ["#8be9fd", "#8be9fd"],  # 13 super blue
# ["#44475a", "#44475a"],  # 14 super dark background
# ["#6272a4", "#6272a4"]   # 15 slate grey
#]

## Graphite
colors = [
 ["#101010", "#101010"],  # 0 background
 ["#b9b9b9", "#b9b9b9"],  # 1 foreground
 ["#101010", "#101010"],  # 2 background lighter
 ["#525252", "#525252"],  # 3 red
 ["#7c7c7c", "#7c7c7c"],  # 4 green
 ["#7c7c7c", "#7c7c7c"],  # 5 yellow
 ["#8e8e8e", "#8e8e8e"],  # 6 blue
 ["#8e8e8e", "#8e8e8e"],  # 7 magenta
 ["#a0a0a0", "#a0a0a0"],  # 8 cyan
 ["#a0a0a0", "#a0a0a0"],  # 9 grey
 ["#686868", "#686868"],  # 10 white
 ["#686868", "#686868"],  # 11 orange
 ["#747474", "#747474"],  # 12 super cyan
 ["#747474", "#747474"],  # 13 super blue
 ["#868686", "#868686"],  # 14 super dark background
 ["#868686", "#868686"]   # 15 slate grey
]

# Nord
# colors = [
# ["#242831", "#242831"],  # 0 background
# ["#f8f8f2", "#f8f8f2"],  # 1 foreground
# ["#3b4252", "#3b4252"],  # 2 background lighter
# ["#bf616a", "#bf616a"],  # 3 red
# ["#a3be8c", "#a3be8c"],  # 4 green
# ["#ebcb8b", "#ebcb8b"],  # 5 yellow
# ["#81a1c1", "#81a1c1"],  # 6 blue
# ["#b48ead", "#b48ead"],  # 7 magenta
# ["#88c0d0", "#88c0d0"],  # 8 cyan
# ["#4c566a", "#4c566a"],  # 9 grey
# ["#e5e9f0", "#e5e9f0"],  # 10 white
# ["#d08770", "#d08770"],  # 11 orange
# ["#8fbcbb", "#8fbcbb"],  # 12 super cyan
# ["#5e81ac", "#5e81ac"],  # 13 super blue
# ["#2e3440", "#2e3440"],  # 14 super dark background
# ["#708090", "#708090"]   # 15 slate grey
# ]

layout_theme = {"border_width": 2,
                "margin": 5,
                "border_focus": colors[9],
                "border_normal": colors[0]
                }

layouts = [
    layout.MonadWide(**layout_theme),
    layout.MonadTall(**layout_theme),
    layout.Columns(**layout_theme),
    layout.Max(**layout_theme),
    # layout.Floating(**layout_theme)
]

widget_defaults = dict(
    # font='CozetteVector Bold',
    font='mononoki Nerd Font Bold',
    fontsize=11,
    padding=5,
    foreground = colors[15],
    background = colors[0]
    )

extension_defaults = widget_defaults.copy()

screens = [
  Screen(
        top=bar.Bar(
            [
              widget.Sep(
                       linewidth = 0,
                       padding = 6,
                       ),
              widget.GroupBox(
                       fontsize = 21,
                       margin_y = 3,
                       margin_x = 0,
                       padding_y = 5,
                       padding_x = 3,
                       borderwidth = 3,
                       inactive = colors[2],
                       active = colors[15],
                       rounded = False,
                       highlight_color = colors[9],
                       highlight_method = "line",
                       this_current_screen_border = colors[15],
                       this_screen_border = colors[15],
                       other_current_screen_border = colors[15],
                       other_screen_border = colors[9],
                       foreground = colors[15],
                       background = colors[0]
                       ),
              widget.Sep(
                       linewidth = 0,
                       padding = 5,
                       ),
              widget.Prompt(
                       prompt = prompt,
                       padding = 6,
                       ),
              widget.Sep(
                       linewidth = 0,
                       padding = 5,
                       ),
              widget.WindowName(
                       padding = 5,
                       fontsize = 10
                       ),
              widget.Sep(
                       linewidth = 0,
                       padding = 5,
                       ),
              widget.TextBox(
                       text = "|",
                       fontsize = 12,
                       foreground = colors[2],
                       ),
              widget.Net(
                      interface = "wlan0",
                      format = '  {down} ↓↑ {up}',
                      padding = 5,
                      ),
              widget.Sep(
                      linewidth = 0,
                      padding = 5,
                      ),
              widget.TextBox(
                      text = "|",
                      fontsize = 12,
                      foreground = colors[2],
                      ),
              widget.Memory(
                      format = '  {MemUsed: .0f}{mm}',
                      mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(terminal + ' -e bpytop')},
                      padding = 5
                      ),
              widget.Sep(
                      linewidth = 0,
                      padding = 5,
                      ),
              widget.TextBox(
                      text = "|",
                      fontsize = 12,
                      foreground = colors[2],
                      ),
              widget.CPU(
                      padding = 5,
                      mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(terminal + ' -e bpytop')},
                      format = '  {load_percent}%',
                      ),
              widget.Sep(
                      linewidth = 0,
                      padding = 5,
                      ),
              widget.TextBox(
                      text = "|",
                      fontsize = 12,
                      foreground = colors[2],
                      ),
              widget.Wttr(
                       padding = 5,
                       location={'Pleszew': 'Pleszew'},
                       format = '  %t'
                       ),
              widget.Sep(
                       linewidth = 0,
                       padding = 5,
                       ),
              widget.TextBox(
                       text = "|",
                       fontsize = 12,
                       foreground = colors[2],
                       ),
              widget.Clock(
                       format = "  %d.%m.%y - %H:%M ",
                       mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(terminal + ' -e calcure')},
                       ),
              widget.Sep(
                       linewidth = 0,
                       padding = 5,
                       ),
              widget.TextBox(
                       text = "|",
                       fontsize = 12,
                       foreground = colors[2],
                       ),
              widget.Systray(),
              widget.CurrentLayoutIcon(
                       custom_icon_paths = [os.path.expanduser("~/.config/qtile/icons")],
                       padding = 5,
                       scale = 0.7
                       ),
              widget.Sep(
                       linewidth = 0,
                       padding = 5,
                       ),
            ], 24, ), ),
    Screen(
        top=bar.Bar(
            [
              widget.Sep(
                       linewidth = 0,
                       padding = 6,
                       ),
              widget.GroupBox(
                       fontsize = 20,
                       margin_y = 3,
                       margin_x = 0,
                       padding_y = 5,
                       padding_x = 3,
                       borderwidth = 3,
                       inactive = colors[2],
                       active = colors[15],
                       rounded = False,
                       highlight_color = colors[9],
                       highlight_method = "line",
                       this_current_screen_border = colors[15],
                       this_screen_border = colors[15],
                       other_current_screen_border = colors[15],
                       other_screen_border = colors[9],
                       foreground = colors[15],
                       background = colors[0]
                       ),
              widget.Sep(
                       linewidth = 0,
                       padding = 5,
                       ),
              widget.WindowName(
                       padding = 5,
                       fontsize = 10
                       ),
              widget.Sep(
                       linewidth = 0,
                       padding = 5,
                       ),
              widget.Spacer(
                       length = bar.STRETCH
                       ),
              widget.Sep(
                       linewidth = 0,
                       padding = 5,
                       ),
              widget.TextBox(
                       text = "|",
                       fontsize = 12,
                       foreground = colors[2],
                       ),
              widget.Clock(
                       format = "  %d.%m.%y - %H:%M ",
                       ),
              widget.Sep(
                       linewidth = 0,
                       padding = 5,
                       ),
              widget.CurrentLayoutIcon(
                       custom_icon_paths = [os.path.expanduser("~/.config/qtile/icons")],
                       padding = 5,
                       scale = 0.7
                       ),
              widget.Sep(
                       linewidth = 0,
                       padding = 5,
                       )
                ], 24), ),
    Screen(
        top=bar.Bar(
            [
              widget.Sep(
                       linewidth = 0,
                       padding = 6,
                       ),
              widget.GroupBox(
                       fontsize = 20,
                       margin_y = 3,
                       margin_x = 0,
                       padding_y = 5,
                       padding_x = 3,
                       borderwidth = 3,
                       inactive = colors[2],
                       active = colors[15],
                       rounded = False,
                       highlight_color = colors[9],
                       highlight_method = "line",
                       this_current_screen_border = colors[15],
                       this_screen_border = colors[15],
                       other_current_screen_border = colors[15],
                       other_screen_border = colors[9],
                       foreground = colors[15],
                       background = colors[0]
                       ),
              widget.Sep(
                       linewidth = 0,
                       padding = 5,
                       ),
              widget.WindowName(
                       padding = 5,
                       fontsize = 10
                       ),
              widget.Sep(
                       linewidth = 0,
                       padding = 5,
                       ),
              widget.Spacer(
                       length = bar.STRETCH
                       ),
              widget.Sep(
                       linewidth = 0,
                       padding = 5,
                       ),
              widget.TextBox(
                       text = "|",
                       fontsize = 12,
                       foreground = colors[2],
                       ),
              widget.Clock(
                       format = "  %d.%m.%y - %H:%M ",
                       ),
              widget.Sep(
                       linewidth = 0,
                       padding = 5,
                       ),
              widget.CurrentLayoutIcon(
                       custom_icon_paths = [os.path.expanduser("~/.config/qtile/icons")],
                       padding = 5,
                       scale = 0.7
                       ),
              widget.Sep(
                       linewidth = 0,
                       padding = 5,
                       )
                ], 24), ),
]

def window_to_prev_group(qtile):
    if qtile.currentWindow is not None:
        i = qtile.groups.index(qtile.currentGroup)
        qtile.currentWindow.togroup(qtile.groups[i - 1].name)

def window_to_next_group(qtile):
    if qtile.currentWindow is not None:
        i = qtile.groups.index(qtile.currentGroup)
        qtile.currentWindow.togroup(qtile.groups[i + 1].name)

def window_to_previous_screen(qtile):
    i = qtile.screens.index(qtile.current_screen)
    if i != 0:
        group = qtile.screens[i - 1].group.name
        qtile.current_window.togroup(group)

def window_to_next_screen(qtile):
    i = qtile.screens.index(qtile.current_screen)
    if i + 1 != len(qtile.screens):
        group = qtile.screens[i + 1].group.name
        qtile.current_window.togroup(group)

def switch_screens(qtile):
    i = qtile.screens.index(qtile.current_screen)
    group = qtile.screens[i - 1].group
    qtile.current_screen.set_group(group)

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
follow_mouse_focus = False
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    *layout.Floating.default_float_rules,
    Match(wm_class='confirmreset'),  # gitk
    Match(wm_class='makebranch'),  # gitk
    Match(wm_class='maketag'),  # gitk
    Match(wm_class='ssh-askpass'),  # ssh-askpass
    Match(wm_class='notification'),
    Match(title='branchdialog'),  # gitk
    Match(title='pinentry'),  # GPG key password entry
])
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

@hook.subscribe.startup_once
def autostart():
    qtile.cmd_spawn("nitrogen --restore &")
    qtile.cmd_spawn("picom &")
    qtile.cmd_spawn("volumeicon &")
    qtile.cmd_spawn("connman-gtk &")
    qtile.cmd_spawn("/usr/bin/emacs --daemon &")
    qtile.cmd_spawn("qbittorrent &")
    qtile.cmd_spawn("keepassxc &")
    qtile.cmd_spawn("xrandr --output eDP1 --mode 1920x1080 --pos 2880x0 --rotate normal --output DP1 --mode 1280x960 --pos 0x0 --rotate left --output HDMI1 --off --output VIRTUAL1 --off --output HDMI-1-0 --primary --mode 1920x1080 --pos 960x0 --rotate normal --output DP-1-0 --off --output DP-1-1 --off")

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
