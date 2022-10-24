## unused widgets
widget.Clock(
        format = "  %d.%m.%y - %H:%M",
        foreground = colors[15],
        ),
widget.CPU(
        padding = 5,
        mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(terminal + ' -e bpytop')},
        foreground = colors[15],
        format = '  {load_percent}%',
        ),
widget.CurrentLayoutIcon(
        custom_icon_paths = [os.path.expanduser("~/.config/qtile/icons")],
        padding = 5,
        scale = 0.7
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
widget.Memory(
        format = '  {MemUsed: .0f}{mm}',
        mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(terminal + ' -e bpytop')},
        foreground = colors[15],
        padding = 5
        ),
widget.Net(
        interface = "wlan0",
        format = '  {down} ↓↑ {up}',
        foreground = colors[15],
        padding = 5,
        ),
widget.Prompt(
        prompt = prompt,
        padding = 6,
        ),
widget.TextBox(
        text = "",
        fontsize = 12,
        foreground = colors[10],
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
widget.Systray(),
widget.Wttr(
        padding = 5,
        location={'Pleszew': 'home'},
        # location={'~51.8960985,17.7865673': 'home'},
        foreground = colors[15],
        # format = '  %t'
        format = '%c%t'
        ),
widget.WindowName(
        padding = 5,
        fontsize = 10
        ),
