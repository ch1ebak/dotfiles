-- ==========================================
-- ▗▖ ▗▖▗▖  ▗▖▗▄▄▖ ▗▄▄▖ ▗▖    ▗▄▖ ▗▖  ▗▖▗▄▄▄
-- ▐▌ ▐▌ ▝▚▞▘ ▐▌ ▐▌▐▌ ▐▌▐▌   ▐▌ ▐▌▐▛▚▖▐▌▐▌  █
-- ▐▛▀▜▌  ▐▌  ▐▛▀▘ ▐▛▀▚▖▐▌   ▐▛▀▜▌▐▌ ▝▜▌▐▌  █
-- ▐▌ ▐▌  ▐▌  ▐▌   ▐▌ ▐▌▐▙▄▄▖▐▌ ▐▌▐▌  ▐▌▐▙▄▄▀
                                         
-- github.com/ch1ebak/dotfiles
-- ==========================================


-- ENVIRONMENT VARIABLES
hl.env("GDK_BACKEND", "wayland,x11,*")	
hl.env("QT_QPA_PLATFORM", "wayland;xcb")
hl.env("XDG_CURRENT_DESKTOP", "Hyprland")
hl.env("XDG_SESSION_TYPE", "wayland")
hl.env("XDG_SESSION_DESKTOP", "Hyprland")
hl.env("GBM_BACKEND", "nvidia-drm")
hl.env("__GLX_VENDOR_LIBRARY_NAME", "nvidia")
hl.env("LIBVA_DRIVER_NAME", "nvidia")

-- MONITORS
hl.monitor({
  output   = "HDMI-A-1",
  mode     = "1920x1080@144",
  position = "1920x0",
  scale    = "1",
})

hl.monitor({
  output   = "eDP-1",
  mode     = "1920x1080@60",
  position = "0x0",
  scale    = "1",
})

-- AUTOSTART
hl.on("hyprland.start", function () 
  hl.exec_cmd("waybar")
  hl.exec_cmd("dbus-update-activation-environment --systemd --all")
  hl.exec_cmd("systemctl --user import-environment QT_QPA_PLATFORMTHEME")
  hl.exec_cmd("swaybg -i ~/.dotfiles/wallpapers/ayu.png")
  hl.exec_cmd("dunst &")
  hl.exec_cmd("hypridle")
  hl.exec_cmd("brightnessctl set 50%")
  hl.exec_cmd("wl-paste --type text --watch cliphist store")
  hl.exec_cmd("easyeffects --gapplication-service &")
  hl.exec_cmd("/usr/bin/emacs --daemon &")
end)

-- SETTINGS
hl.config({
  general = {
    gaps_in  = 3,
    gaps_out = 5,
    border_size = 2,
    col = {
      active_border   = "rgba(cba6f7ff)",
      inactive_border = "rgba(595959aa)",
    },
    resize_on_border = false,
    allow_tearing = false,
    layout = "master",
  },
  misc = {
    force_default_wallpaper = 0,
    disable_hyprland_logo = true,
  },
  input = {
    kb_layout  = "pl",
    follow_mouse = 0,
    sensitivity = 0,
    touchpad = {
      natural_scroll = false,
    },
  },
	cursor = {
		no_hardware_cursors = true,
		no_warps = true,
	},
	binds {
		workspace_back_and_forth = 1,
	},
  decoration = {
    rounding       = 5,
    rounding_power = 2,
    active_opacity   = 1.0,
    inactive_opacity = 1.0,
    shadow = {
      enabled      = true,
      range        = 4,
      render_power = 3,
      color        = "rgba(313244ff)",
    },
    blur = {
      enabled   = true,
      size      = 8,
      passes    = 1,
      vibrancy  = 0.1696,
    },
  },
  animations = {
    enabled = true,
  },
  master = {
    new_status = "slave",
		new_on_top = "false",
		orientation = left,
		mfact = 0.50,
  },
  scrolling = {
    fullscreen_on_one_column = true,
  },
})

-- Animations
hl.curve("easeOutQuint",   { type = "bezier", points = { {0.23, 1},    {0.32, 1}    } })
hl.curve("easeInOutCubic", { type = "bezier", points = { {0.65, 0.05}, {0.36, 1}    } })
hl.curve("linear",         { type = "bezier", points = { {0, 0},       {1, 1}       } })
hl.curve("almostLinear",   { type = "bezier", points = { {0.5, 0.5},   {0.75, 1}    } })
hl.curve("quick",          { type = "bezier", points = { {0.15, 0},    {0.1, 1}     } })
hl.curve("easy",           { type = "spring", mass = 1, stiffness = 71.2633, dampening = 15.8273644 })
hl.animation({ leaf = "global",        enabled = true,  speed = 10,   bezier = "default" })
hl.animation({ leaf = "border",        enabled = true,  speed = 5.39, bezier = "easeOutQuint" })
hl.animation({ leaf = "windows",       enabled = true,  speed = 4.79, spring = "easy" })
hl.animation({ leaf = "windowsIn",     enabled = true,  speed = 4.1,  spring = "easy",         style = "popin 87%" })
hl.animation({ leaf = "windowsOut",    enabled = true,  speed = 1.49, bezier = "linear",       style = "popin 87%" })
hl.animation({ leaf = "fadeIn",        enabled = true,  speed = 1.73, bezier = "almostLinear" })
hl.animation({ leaf = "fadeOut",       enabled = true,  speed = 1.46, bezier = "almostLinear" })
hl.animation({ leaf = "fade",          enabled = true,  speed = 3.03, bezier = "quick" })
hl.animation({ leaf = "layers",        enabled = true,  speed = 3.81, bezier = "easeOutQuint" })
hl.animation({ leaf = "layersIn",      enabled = true,  speed = 4,    bezier = "easeOutQuint", style = "fade" })
hl.animation({ leaf = "layersOut",     enabled = true,  speed = 1.5,  bezier = "linear",       style = "fade" })
hl.animation({ leaf = "fadeLayersIn",  enabled = true,  speed = 1.79, bezier = "almostLinear" })
hl.animation({ leaf = "fadeLayersOut", enabled = true,  speed = 1.39, bezier = "almostLinear" })
hl.animation({ leaf = "workspaces",    enabled = true,  speed = 1.94, bezier = "almostLinear", style = "fade" })
hl.animation({ leaf = "workspacesIn",  enabled = true,  speed = 1.21, bezier = "almostLinear", style = "fade" })
hl.animation({ leaf = "workspacesOut", enabled = true,  speed = 1.94, bezier = "almostLinear", style = "fade" })
hl.animation({ leaf = "zoomFactor",    enabled = true,  speed = 7,    bezier = "quick" })

-- WINDOW RULES
local suppressMaximizeRule = hl.window_rule({
    name  = "suppress-maximize-events",
    match = { class = ".*" },
    suppress_event = "maximize",
})

hl.window_rule({
  name  = "fix-xwayland-drags",
  match = {
    class      = "^$",
    title      = "^$",
    xwayland   = true,
    float      = true,
    fullscreen = false,
    pin        = false,
  },
  no_focus = true,
})

hl.window_rule({
  name = "feh",
  match = {
    class = "feh",
    fullscreen = false,
  },
})

hl.window_rule({
  name = "Ferdium",
  match = {
    class = "Ferdium",
		workspace = 1,
  },
})

hl.window_rule({
  name = "Firefox",
  match = {
    class = "firefox",
		workspace = 2,
  },
})

hl.window_rule({
  name = "Emacs",
  match = {
    class = "Emacs",
		workspace = 3,
  },
})

hl.window_rule({
  name = "Wezterm",
  match = {
    class = "org.wezfurlong.wezterm",
		workspace = 4,
  },
})

hl.window_rule({
  name = "pcmanfm",
  match = {
    class = "pcmanfm",
		workspace = 5,
  },
})

hl.window_rule({
  name = "Rawtherapee",
  match = {
    class = "rawtherapee",
		workspace = 6,
  },
})

hl.window_rule({
  name = "Vial",
  match = {
    class = "Vial",
		workspace = 6,
  },
})

hl.window_rule({
  name = "Calibre",
  match = {
    class = "calibre-gui",
		workspace = 6,
  },
})

hl.window_rule({
  name = "nwg-look",
  match = {
    class = "nwg-look",
		workspace = 6,
  },
})

hl.window_rule({
  name = "QBittorrent",
  match = {
    class = "org.qbittorrent.qBittorrent",
		workspace = 6,
  },
})

hl.window_rule({
  name = "Nexus Mods",
  match = {
    class = "NexusMods.App",
		workspace = 6,
  },
})

hl.window_rule({
  name = "steam",
  match = {
    class = "steam",
		workspace = 7,
  },
})

hl.window_rule({
  name = "heroic",
  match = {
    class = "heroic",
		workspace = 7,
  },
})

hl.window_rule({
  name = "EasyEffects",
  match = {
    class = "com.github.wwmm.easyeffects",
		workspace = 8,
  },
})

hl.window_rule({
  name = "mpv",
  match = {
    class = "mpv",
		workspace = 9,
  },
})

-- KEYBINDINGS
local mainMod = "SUPER"

hl.bind(mainMod .. " + ", hl.dsp.exec_cmd(""))
hl.bind(mainMod .. " + SHIFT + ", hl.dsp.exec_cmd(""))

-- Apps
hl.bind(mainMod .. " + RETURN", hl.dsp.exec_cmd("wezterm"))
hl.bind(mainMod .. " + S", hl.dsp.exec_cmd("rofi -show drun"))
hl.bind(mainMod .. " + W", hl.dsp.exec_cmd("firefox"))
hl.bind(mainMod .. " + SHIFT + W", hl.dsp.exec_cmd("firefox --private-window"))
hl.bind(mainMod .. " + D", hl.dsp.exec_cmd("emacsclient -c -a 'emacs'"))
hl.bind(mainMod .. " + SHIFT + D", hl.dsp.exec_cmd("sh /home/$USER/.local/scripts/emacs-restart"))
hl.bind(mainMod .. " + A", hl.dsp.exec_cmd("wezterm -e yazi"))
-- hl.bind(mainMod .. " + A", hl.dsp.exec_cmd("emacsclient -c -a 'emacs' --eval '(dired nil)'"))
hl.bind(mainMod .. " + SHIFT + A", hl.dsp.exec_cmd("pcmanfm"))
hl.bind(mainMod .. " + U", hl.dsp.exec_cmd("sh /home/$USER/Projekty/scripts/rofi-utilities"))
hl.bind(mainMod .. " + P", hl.dsp.exec_cmd("hyprlock"))
hl.bind(mainMod .. " + SHIFT + P", hl.dsp.exec_cmd("rofi -m 1 -show power-menu -modi power-menu:~/.config/rofi/modules/rofi-power-menu"))

-- App Control
local closeWindowBind = hl.bind("SUPER + Q", hl.dsp.window.close())
hl.bind(mainMod .. "CTRL + Q", hl.dsp.exec_cmd("command -v hyprshutdown >/dev/null 2>&1 && hyprshutdown || hyprctl dispatch 'hl.dsp.exit()'"))
hl.bind(mainMod .. " + CTRL + ", hl.dsp.exec_cmd("killall waybar && waybar & disown"))

-- Layouts
hl.bind(mainMod .. " + TAB", hl.dsp.layout("togglesplit"))
hl.bind(mainMod .. " + V", hl.dsp.window.float({ action = "toggle" }))

-- Switch between monitors

-- Scroll through workspaces
hl.bind(mainMod .. " + K", hl.dsp.focus({ workspace = "e+1" }))
hl.bind(mainMod .. " + J", hl.dsp.focus({ workspace = "e-1" }))

-- Move focus with mainMod + arrow keys
hl.bind(mainMod .. " + H", hl.dsp.focus({ direction = "left" }))
hl.bind(mainMod .. " + L", hl.dsp.focus({ direction = "right" }))

-- Switch workspaces with mainMod + [0-9]
-- Move active window to a workspace with mainMod + SHIFT + [0-9]
for i = 1, 10 do
    local key = i % 10 -- 10 maps to key 0
    hl.bind(mainMod .. " + " .. key,             hl.dsp.focus({ workspace = i}))
    hl.bind(mainMod .. " + SHIFT + " .. key,     hl.dsp.window.move({ workspace = i }))
end

-- Move windows

-- Resize windows

-- Mouse
hl.bind(mainMod .. " + mouse:272", hl.dsp.window.drag(),   { mouse = true })
hl.bind(mainMod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })
