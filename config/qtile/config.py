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
terminal = guess_terminal("kitty")
prompt = "{0}@{1}: ".format(os.environ["USER"], socket.gethostname())

keys = [
     # Apps
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    Key([mod, "control"], "Return", lazy.spawn("emacsclient -c -a 'emacs' --eval '(+vterm/here nil)'"), desc="Launch VTerm"),

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

    Key([mod], "w", lazy.spawn("firefox-esr"), desc="Launch Firefox"),
    Key([mod, "shift"], "w", lazy.spawn("brave"), desc="Launch Brave"),

    # App control
    Key([mod], "q", lazy.window.kill(), desc="Kill focused window"),
    Key([mod, "control"], "r", lazy.restart(), desc="Restart Qtile"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),

    # Switch between screens
    Key([mod], "z", lazy.to_screen(2)),
    Key([mod], "c", lazy.to_screen(1)),
    Key([mod], "x", lazy.to_screen(0)),

    # Switch between windows
    Key([mod], "h", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "l", lazy.layout.up(), desc="Move focus up"),

    # Switch between groups
    Key([mod], 'j', lazy.screen.prev_group(skip_managed=True, )),
    Key([mod], 'k', lazy.screen.next_group(skip_managed=True, )),

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
    Key(["shift", "control"], "Return", lazy.layout.toggle_split(),
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
       Group("3", label="", layout='monadtall'),
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

# Catppuccin
#colors = [
 #["#1E1E2E", "#1E1E2E"],  # 0
 #["#89b4fa", "#89b4fa"],  # 1
 #["#313244", "#313244"],  # 2
 #["#f38ba8", "#f38ba8"],  # 3
 #["#94e2d5", "#94e2d5"],  # 4
 #["#f9e2af", "#f9e2af"],  # 5
 #["#b4befe", "#b4befe"],  # 6
 #["#f5e0dc", "#f5e0dc"],  # 7
 #["#f5c2e7", "#f5c2e7"],  # 8
 #["#44475a", "#44475a"],  # 9
 #["#89dceb", "#89dceb"],  # 10
 #["#fab387", "#fab387"],  # 11
 #["#74c7ec", "#74c7ec"],  # 12
 #["#a6e3a1", "#a6e3a1"],  # 13
 #["#cba6f7", "#cba6f7"],  # 14
 #["#cdd6f4", "#cdd6f4"]   # 15
#]

## Dracula
#colors = [
 #["#282a36", "#282a36"],  # 0
 #["#f8f8f2", "#f8f8f2"],  # 1
 #["#44475a", "#44475a"],  # 2
 #["#ff5555", "#ff5555"],  # 3
 #["#50fa7b", "#50fa7b"],  # 4
 #["#f1fa8c", "#f1fa8c"],  # 5
 #["#8be9fd", "#8be9fd"],  # 6
 #["#bfbfbf", "#bfbfbf"],  # 7
 #["#8be9fd", "#8be9fd"],  # 8
 #["#6272a4", "#6272a4"],  # 9
 #["#ff79c6", "#ff79c6"],  # 10
 #["#ffb86c", "#ffb86c"],  # 11
 #["#8be9fd", "#8be9fd"],  # 12
 #["#8be9fd", "#8be9fd"],  # 13
 #["#BD93F9", "#BD93F9"],  # 14
 #["#f8f8f2", "#f8f8f2"]   # 15
#]

## Gruvbox
#colors = [
 #["#282828", "#282828"],  # 0
 #["#ebdbb2", "#ebdbb2"],  # 1
 #["#928374", "#928374"],  # 2
 #["#b16286", "#b16286"],  # 3
 #["#d79921", "#d79921"],  # 4
 #["#d3869b", "#d3869b"],  # 5
 #["#458588", "#458588"],  # 6
 #["#a89984", "#a89984"],  # 7
 #["#458588", "#458588"],  # 8
 #["#83a598", "#83a598"],  # 9
 #["#cc241d", "#cc241d"],  # 10
 #["#fabd2f", "#fabd2f"],  # 11
 #["#458588", "#458588"],  # 12
 #["#458588", "#458588"],  # 13
 #["#98971a", "#98971a"],  # 14
 #["#ebdbb2", "#ebdbb2"]   # 15
#]

# Nord
#colors = [
 #["#242831", "#242831"],  # 0
 #["#81a1c1", "#81a1c1"],  # 1
 #["#2e3440", "#2e3440"],  # 2
 #["#bf616a", "#bf616a"],  # 3
 #["#a3be8c", "#a3be8c"],  # 4
 #["#ebcb8b", "#ebcb8b"],  # 5
 #["#f8f8f2", "#f8f8f2"],  # 6
 #["#3b4252", "#3b4252"],  # 7
 #["#88c0d0", "#88c0d0"],  # 8
 #["#4c566a", "#4c566a"],  # 9
 #["#e5e9f0", "#e5e9f0"],  # 10
 #["#8fbcbb", "#8fbcbb"],  # 11
 #["#d08770", "#d08770"],  # 12
 #["#5e81ac", "#5e81ac"],  # 13
 #["#b48ead", "#b48ead"],  # 14
 #["#708090", "#708090"]   # 15
#]

## One dark
colors = [
 ["#282c34", "#282c34"],  # 0
 ["#bbc2cf", "#bbc2cf"],  # 1
 ["#21242b", "#21242b"],  # 2
 ["#ff6c6b", "#ff6c6b"],  # 3
 ["#98be65", "#98be65"],  # 4
 ["#ECBE7B", "#ECBE7B"],  # 5
 ["#51afef", "#51afef"],  # 6
 ["#5B6268", "#5B6268"],  # 7
 ["#da8548", "#da8548"],  # 8
 ["#5B6268", "#5B6268"],  # 9
 ["#c678dd", "#c678dd"],  # 10
 ["#a9a1e1", "#a9a1e1"],  # 11
 ["#51afef", "#51afef"],  # 12
 ["#2257A0", "#2257A0"],  # 13
 ["#51afef", "#51afef"],  # 14
 ["#bbc2cf", "#bbc2cf"]   # 15
]

layout_theme = {"border_width": 2,
                "margin": 8,
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
              widget.TextBox(
                       text = "",
                       fontsize = 12,
                       foreground = colors[10],
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
                       location={'Pleszew': 'Pleszew'},
                       foreground = colors[15],
                       format = '  %t'
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
              widget.TextBox(
                       text = "",
                       fontsize = 12,
                       foreground = colors[10],
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
              widget.TextBox(
                       text = "",
                       fontsize = 12,
                       foreground = colors[10],
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
    qtile.cmd_spawn("picom --experimental-backend -b")
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
