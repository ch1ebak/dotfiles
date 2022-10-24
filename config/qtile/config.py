###################################
### https://github.com/ch1ebak/ ###
###################################

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

# from colors.catppuccin import colors
from colors.dracula import colors
# from colors.everforest import colors
# from colors.grayscale import colors
# from colors.gruvbox import colors
# from colors.nord import colors
# from colors.onedark import colors
# from colors.solarizeddark import colors
# from colors.tokyonight import colors

mod = "mod4"
# terminal = guess_terminal("kitty")
terminal = guess_terminal("alacritty")
prompt = "{0}@{1}: ".format(os.environ["USER"], socket.gethostname())

keys = [
     # Apps
    Key([mod], "Return", lazy.spawn("alacritty"), desc="Launch terminal"),
    Key([mod, "shift"], "Return", lazy.spawn("kitty"), desc="Launch vterm"),
    # Key([mod, "shift"], "Return", lazy.spawn("emacsclient -c -a 'emacs' --eval '(+vterm/here nil)'"), desc="Launch vterm"),

    Key([mod], "a", lazy.spawn("emacsclient -c -a 'emacs' --eval '(dired nil)'"), desc="Launch Dired"),
    Key([mod, "shift"], "a", lazy.spawn("pcmanfm"), desc="Launch pcmanfm"),

    Key([mod], "d", lazy.spawn("signal-desktop"), desc="Launch signal"),
    Key([mod, "shift"], "d", lazy.spawn("ferdium"), desc="Launch Ferdium"),
    Key([mod, "control"], "d", lazy.spawn("emacsclient -c -a 'emacs' --eval '(mu4e nil)'"), desc="Launch Mail"),

    Key([mod], "e", lazy.spawn("emacsclient -c -a 'emacs'"), desc='Launch Emacsclient'),
    Key([mod, "shift"], "e", lazy.spawn("emacs"), desc='Launch emacs'),
    Key([mod, "control"], "e", lazy.spawn("kitty -e emacs"), desc="Launch Emacs Terminal"),

    Key([mod], "g", lazy.spawn("steam"), desc='Launch Steam'),
    Key([mod, "shift"], "g", lazy.spawn("lutris"), desc='Launch Lutris'),
    Key([mod, "control"], "g", lazy.spawn("heroic"), desc='Launch Heroic Game Launcher'),

    Key([mod], "m", lazy.spawn("kitty -e ncmpcpp"), desc="Launch ncmpcpp"),
    Key([mod, "shift"], "m", lazy.spawn("rhythmbox"), desc="Launch rhythmbox"),

    Key([mod], "p", lazy.spawn("slock"), desc="Launch lock screen"),
    Key([mod, "shift"], "p", lazy.spawn("rofi -show power-menu -modi power-menu:~/.config/rofi/modules/rofi-power-menu"), desc="Launch Rofi Power Menu"),

    Key([mod], "s", lazy.spawn("rofi -show drun"), desc="Launch rofi"),

    Key([mod], "w", lazy.spawn("firefox"), desc="Launch Firefox"),
    Key([mod, "shift"], "w", lazy.spawn("/usr/bin/firefox --private-window"), desc="Launch Firefox Private"),

    # App control
    Key([mod], "q", lazy.window.kill(), desc="Kill focused window"),
    Key([mod, "control"], "r", lazy.restart(), desc="Restart Qtile"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),

    # Switch between screens (3 monitors)
    # Key([mod], "z", lazy.to_screen(2)),
    # Key([mod], "c", lazy.to_screen(1)),
    # Key([mod], "x", lazy.to_screen(0)),

    # Switch between screens (2 monitors)
    Key([mod], "z", lazy.to_screen(1)),
    Key([mod], "x", lazy.to_screen(0)),

    # Switch between windows
    Key([mod], "h", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "l", lazy.layout.next(), desc="Move focus up"),

    # Switch between groups
    Key([mod], 'j', lazy.screen.prev_group(skip_managed=True, )),
    Key([mod], 'k', lazy.screen.next_group(skip_managed=True, )),

    # Move windows
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(),
        desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(),
        desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(),
        desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),

    # Grow windows.
    Key([mod, "control"], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    Key([mod, "control"], "h", lazy.layout.grow_left(),
        desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(),
        desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.shrink(),
        desc="Shrink window"),
    Key([mod, "control"], "k", lazy.layout.grow(), desc="Grow window"),

    # Layouts
    Key(["shift", "control"], "Return", lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack"),
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts")
]

# Icons-dots, for 2 monitors
groups = [
       Group("1", label="", layout='monadtall'),
       Group("2", label="", layout='monadtall'),
       Group("3", label="", layout='monadtall'),
       Group("4", label="", layout='monadtall'),
       Group("5", label="", layout='monadtall'),
       Group("6", label="", layout='monadtall'),
       Group("7", label="", layout='monadtall'),
       Group("8", label="", layout='monadwide', matches=[Match(wm_class=["vlc", "mpv", "rhythmbox"])]),
       Group("9", label="", layout='monadtall', matches=[Match(wm_class=["signal", "ferdium"])])
       ]

for i in range(len(groups)):
    keys.append(Key([mod], str((i)), lazy.group[str(i)].toscreen()))
    keys.append(
        Key([mod, "shift"], str((i)), lazy.window.togroup(str(i), switch_group=True))
    )

layout_theme = {"border_width": 2,
                "margin": 4,
                "border_focus": colors[14],
                "border_normal": colors[0]
                }

layouts = [
    layout.MonadWide(**layout_theme),
    layout.MonadTall(**layout_theme),
    layout.Columns(**layout_theme),
    layout.Max(**layout_theme),
]

widget_defaults = dict(
    font='JetBrainsMono Nerd Font',
    fontsize=10,
    padding=5,
    foreground = colors[15],
    background = colors[0]
    )

screens = [
 Screen(
         top=bar.Bar(
             [
             widget.Sep(
                     linewidth = 0,
                     padding = 5,
                     ),
             widget.TextBox(
                     text = "",
                     fontsize = 12,
                     foreground = colors[10],
                     ),
             widget.GroupBox(
                     fontsize = 12,
                     font='Font Awesome',
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
                     this_screen_border = colors[7],
                     other_current_screen_border = colors[7],
                     other_screen_border = colors[15],
                     foreground = colors[15],
                     background = colors[0]
                     ),
             widget.TextBox(
                     text = "",
                     fontsize = 12,
                     foreground = colors[4],
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
                     text = "",
                     fontsize = 12,
                     foreground = colors[10],
                     ),
             widget.Net(
                     interface = "wlan0",
                     format = '  {down} ↓↑ {up}',
                     foreground = colors[15],
                     padding = 5,
                     ),
             widget.TextBox(
                     text = "",
                     fontsize = 12,
                     foreground = colors[4],
                     ),
             widget.Sep(
                     linewidth = 0,
                     padding = 5,
                     ),
             widget.TextBox(
                     text = "",
                     fontsize = 12,
                     foreground = colors[10],
                     ),
             widget.Memory(
                     format = '  {MemUsed: .0f}{mm}',
                     mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(terminal + ' -e bpytop')},
                     foreground = colors[15],
                     padding = 5
                     ),
             widget.TextBox(
                     text = "",
                     fontsize = 12,
                     foreground = colors[4],
                     ),
             widget.Sep(
                     linewidth = 0,
                     padding = 5,
                     ),
             widget.TextBox(
                     text = "",
                     fontsize = 12,
                     foreground = colors[10],
                     ),
             widget.CPU(
                     padding = 5,
                     mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(terminal + ' -e bpytop')},
                     foreground = colors[15],
                     format = '  {load_percent}%',
                     ),
             widget.TextBox(
                     text = "",
                     fontsize = 12,
                     foreground = colors[4],
                     ),
             widget.Sep(
                     linewidth = 0,
                     padding = 5,
                     ),
             widget.TextBox(
                     text = "",
                     fontsize = 12,
                     foreground = colors[10],
                     ),
             widget.Wttr(
                     padding = 5,
                     location={'Pleszew': 'home'},
                     # location={'~51.8960985,17.7865673': 'home'},
                     foreground = colors[15],
                     format = '  %t'
                     # format = '%c%t'
                     ),
             widget.TextBox(
                     text = "",
                     fontsize = 12,
                     foreground = colors[4],
                     ),
             widget.Sep(
                     linewidth = 0,
                     padding = 5,
                     ),
             widget.TextBox(
                     text = "",
                     fontsize = 12,
                     foreground = colors[10],
                     ),
             widget.Clock(
                     format = "  %d.%m.%y - %H:%M",
                     foreground = colors[15],
                     ),
             widget.TextBox(
                     text = "",
                     fontsize = 12,
                     foreground = colors[4],
                     ),
             widget.Sep(
                     linewidth = 0,
                     padding = 5,
                     ),
             widget.TextBox(
                     text = "",
                     fontsize = 12,
                     foreground = colors[10],
                     ),
             widget.Systray(),
             widget.TextBox(
                     text = "",
                     fontsize = 12,
                     foreground = colors[4],
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
                     ),
            ], 24, ), ),
 Screen(
         top=bar.Bar(
             [
             widget.Sep(
                     linewidth = 0,
                     padding = 5,
                     ),
             widget.TextBox(
                     text = "",
                     fontsize = 12,
                     foreground = colors[10],
                     ),
             widget.GroupBox(
                     fontsize = 12,
                     font='Font Awesome',
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
                     this_screen_border = colors[7],
                     other_current_screen_border = colors[7],
                     other_screen_border = colors[15],
                     foreground = colors[15],
                     background = colors[0]
                     ),
             widget.TextBox(
                     text = "",
                     fontsize = 12,
                     foreground = colors[4],
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
                     text = "",
                     fontsize = 12,
                     foreground = colors[10],
                     ),
             widget.Clock(
                     format = "  %d.%m.%y - %H:%M",
                     foreground = colors[15],
                     ),
             widget.TextBox(
                     text = "",
                     fontsize = 12,
                     foreground = colors[4],
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
                     ),
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
    Match(title='origin.exe'),  # GPG key password entry
])
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

@hook.subscribe.startup_once
def autostart():
    ## all three
    # qtile.cmd_spawn("xrandr --output eDP1 --mode 1920x1080 --pos 2880x0 --rotate normal --output DP1 --mode 1280x960 --pos 0x0 --rotate left --output HDMI1 --off --output VIRTUAL1 --off --output HDMI-1-0 --primary --mode 1920x1080 --pos 960x0 --rotate normal --output DP-1-0 --off --output DP-1-1 --off")
    ## left-center
    # qtile.cmd_spawn("xrandr --output eDP1 --off --output DP1 --mode 1280x960 --pos 0x0 --rotate left --output HDMI1 --off --output VIRTUAL1 --off --output HDMI-1-0 --primary --mode 1920x1080 --pos 960x0 --rotate normal --output DP-1-0 --off --output DP-1-1 --off")
    ## left-center2
    qtile.cmd_spawn("xrandr --output eDP1 --off --output DP1 --mode 1280x1024 --pos 0x0 --rotate left --output HDMI1 --off --output VIRTUAL1 --off --output HDMI-1-0 --primary --mode 1920x1080 --pos 1024x0 --rotate normal --output DP-1-0 --off --output DP-1-1 --off")
    # qtile.cmd_spawn("xrandr --output eDP1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output DP1 --off --output HDMI1 --off --output VIRTUAL1 --off --output HDMI-1-0 --off --output DP-1-0 --off --output DP-1-1 --off")
    qtile.cmd_spawn("nitrogen --restore &")
    qtile.cmd_spawn("picom --experimental-backend -b")
    qtile.cmd_spawn("/usr/bin/emacs --daemon &")
    qtile.cmd_spawn("connman-gtk &")
    qtile.cmd_spawn("volumeicon &")
    qtile.cmd_spawn("keepassxc &")
    qtile.cmd_spawn("/usr/bin/pipewire &")
    qtile.cmd_spawn("/usr/bin/pipewire-pulse &")
    qtile.cmd_spawn("/usr/bin/pipewire-alsa &")
    qtile.cmd_spawn("/usr/bin/pipewire-jack &")
    qtile.cmd_spawn("/usr/bin/wireplumber &")
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
