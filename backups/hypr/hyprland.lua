-- ==========================================
-- ▗▖ ▗▖▗▖  ▗▖▗▄▄▖ ▗▄▄▖ ▗▖    ▗▄▖ ▗▖  ▗▖▗▄▄▄
-- ▐▌ ▐▌ ▝▚▞▘ ▐▌ ▐▌▐▌ ▐▌▐▌   ▐▌ ▐▌▐▛▚▖▐▌▐▌  █
-- ▐▛▀▜▌  ▐▌  ▐▛▀▘ ▐▛▀▚▖▐▌   ▐▛▀▜▌▐▌ ▝▜▌▐▌  █
-- ▐▌ ▐▌  ▐▌  ▐▌   ▐▌ ▐▌▐▙▄▄▖▐▌ ▐▌▐▌  ▐▌▐▙▄▄▀
                                         
-- github.com/ch1ebak/dotfiles
-- ==========================================

-- ENVIRONMENT VARIABLES
-- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Environment-variables/
hl.env("QT_QPA_PLATFORMTHEME", "qt6ct")
hl.env("XDG_SESSION_TYPE", "wayland")
hl.env("LIBVA_DRIVER_NAME", "nvidia")
hl.env("GBM_BACKEND", "nvidia-drm")
hl.env("GBM_BACKEND", "nvidia-drm")
hl.env("__GLX_VENDOR_LIBRARY_NAME", "nvidia")
hl.env("LD_LIBRARY_PATH", "/run/opengl-driver/lib:/run/opengl-driver-32/lib btop")

-- MONITORS
-- See https://wiki.hypr.land/Configuring/Basics/Monitors/
hl.monitor({
  output   = "HDMI-A-1",
  mode     = "1920x1080@144",
  position = "1920x0",
  scale    = "1",
})

hl.monitor({
  output   = "eDP-2",
  mode     = "1920x1080@60",
  position = "0x0",
  scale    = "1",
})

-- AUTOSTART
-- See https://wiki.hypr.land/Configuring/Basics/Autostart/
hl.on("hyprland.start", function () 
  hl.exec_cmd("waybar &")
  hl.exec_cmd("hypridle &")
  hl.exec_cmd("dunst &")
  hl.exec_cmd("nm-applet &")
  hl.exec_cmd("dbus-update-activation-environment --systemd --all")
  hl.exec_cmd("brightnessctl --device=intel_backlight set 40%")
  hl.exec_cmd("wl-paste --type text --watch cliphist store")
  hl.exec_cmd("pkill swaybg; swaybg -m fill -i $(shuf -e -n1 $HOME/Obrazy/wallpapers/dracula/*) &")
  hl.exec_cmd("easyeffects --gapplication-service &")
end)

-- LOOK AND FEEL

-- Theme
require("themes/everforest")

-- Settings
hl.config({
	-- Refer to https://wiki.hypr.land/Configuring/Basics/Variables/
  general = {
    gaps_in  = 5,
    gaps_out = 5,
    border_size = 2,
    col = {
      active_border = colors.active,
      inactive_border = colors.inactive,
    },
    resize_on_border = true,
    allow_tearing = false,
    layout = "master",
  },
  decoration = {
    rounding       = 5,
    rounding_power = 2,
    active_opacity   = 1.0,
    inactive_opacity = 1.0,
    blur = {
      enabled   = true,
      size      = 3,
      passes    = 1,
      vibrancy  = 0.1696,
    },
  },
  animations = {
    enabled = true,
  },
	-- See https://wiki.hypr.land/Configuring/Layouts/Master-Layout/ for more
  master = {
    new_status = "slave",
		mfact = 0.50,
  },
	-- See https://wiki.hypr.land/Configuring/Layouts/Scrolling-Layout/ for more
  scrolling = {
    fullscreen_on_one_column = true,
  },
  misc = {
    force_default_wallpaper = -1,
    disable_hyprland_logo = true,
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
hl.animation({ leaf = "windowsIn",     enabled = true,  speed = 4.1,  spring = "easy",         style = "slide" })
hl.animation({ leaf = "windowsOut",    enabled = true,  speed = 1.49, bezier = "linear",       style = "slide" })
hl.animation({ leaf = "fadeIn",        enabled = true,  speed = 1.73, bezier = "almostLinear" })
hl.animation({ leaf = "fadeOut",       enabled = true,  speed = 1.46, bezier = "almostLinear" })
hl.animation({ leaf = "fade",          enabled = true,  speed = 3.03, bezier = "quick" })
hl.animation({ leaf = "layers",        enabled = true,  speed = 3.81, bezier = "easeOutQuint" })
hl.animation({ leaf = "layersIn",      enabled = true,  speed = 4,    bezier = "easeOutQuint", style = "slide" })
hl.animation({ leaf = "layersOut",     enabled = true,  speed = 1.5,  bezier = "linear",       style = "slide" })
hl.animation({ leaf = "fadeLayersIn",  enabled = true,  speed = 1.79, bezier = "almostLinear" })
hl.animation({ leaf = "fadeLayersOut", enabled = true,  speed = 1.39, bezier = "almostLinear" })
hl.animation({ leaf = "workspaces",    enabled = true,  speed = 1.94, bezier = "almostLinear", style = "slide" })
hl.animation({ leaf = "workspacesIn",  enabled = true,  speed = 1.21, bezier = "almostLinear", style = "slide" })
hl.animation({ leaf = "workspacesOut", enabled = true,  speed = 1.94, bezier = "almostLinear", style = "slide" })
hl.animation({ leaf = "zoomFactor",    enabled = true,  speed = 7,    bezier = "quick" })


-- WINDOW AND WORKSPACE RULES
-- See https://wiki.hypr.land/Configuring/Basics/Window-Rules/
-- and https://wiki.hypr.land/Configuring/Basics/Workspace-Rules/

-- Ignore maximize requests from all apps. You'll probably like this.
local suppressMaximizeRule = hl.window_rule({
  name  = "suppress-maximize-events",
  match = { class = ".*" },
  suppress_event = "maximize",
})
-- suppressMaximizeRule:set_enabled(false)

-- Fix some dragging issues with XWayland
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

hl.window_rule({ match = { class = "ferdium" }, workspace = 1 })
hl.window_rule({ match = { class = "app.zen_browser.zen" }, workspace = 2 })
hl.window_rule({ match = { class = "Emacs" }, workspace = 3 })
hl.window_rule({ match = { class = "org.wezfurlong.wezterm" }, workspace = 4 })
hl.window_rule({ match = { class = "pcmanfm" }, workspace = 5 })
hl.window_rule({ match = { class = "rawtherapee" }, workspace = 6 })
hl.window_rule({ match = { class = "Vial" }, workspace = 6 })
hl.window_rule({ match = { class = "calibre-gui" }, workspace = 6 })
hl.window_rule({ match = { class = "nwg-look" }, workspace = 6 })
hl.window_rule({ match = { class = "org.qbittorrent.qBittorrent" }, workspace = 6 })
hl.window_rule({ match = { class = "NexusMods.App" }, workspace = 6 })
hl.window_rule({ match = { class = "steam" }, workspace = 7 })
hl.window_rule({ match = { class = "heroic" }, workspace = 7 })
hl.window_rule({ match = { class = "com.github.wwmm.easyeffects" }, workspace = 8 })
hl.window_rule({ match = { class = "mpv" }, workspace = 9 })


-- KEYS
hl.config({
  input = {
    follow_mouse = 0,
    sensitivity = 0,
    touchpad = {
      natural_scroll = false,
    },
  },
	cursor = {
		no_warps = true,
	},
})

hl.gesture({
  fingers = 3,
  direction = "horizontal",
  action = "workspace"
})

-- Keybindings
-- https://wiki.hypr.land/Configuring/Basics/Binds/
local mainMod = "SUPER"

-- Apps
hl.bind(mainMod .. " + ", hl.dsp.exec_cmd(""))
hl.bind(mainMod .. " + SHIFT + ", hl.dsp.exec_cmd(""))

hl.bind(mainMod .. " + Return", hl.dsp.exec_cmd("wezterm"))
hl.bind(mainMod .. " + S", hl.dsp.exec_cmd("rofi -show drun"))
hl.bind(mainMod .. " + W", hl.dsp.exec_cmd("app.zen_browser.zen"))
hl.bind(mainMod .. " + SHIFT + W", hl.dsp.exec_cmd("app.zen_browser.zen --private-window"))
hl.bind(mainMod .. " + D", hl.dsp.exec_cmd("emacsclient -c -a 'emacs'"))
hl.bind(mainMod .. " + SHIFT + D", hl.dsp.exec_cmd("/home/$USER/.local/scripts/emacs-restart"))
hl.bind(mainMod .. " + A", hl.dsp.exec_cmd("wezterm -e yazi"))
-- hl.bind(mainMod .. " + A", hl.dsp.exec_cmd("emacsclient -c -a 'emacs' --eval '(dired nil)'"))
hl.bind(mainMod .. " + SHIFT + A", hl.dsp.exec_cmd("pcmanfm"))
hl.bind(mainMod .. " + U", hl.dsp.exec_cmd("/home/$USER/Projekty/scripts/rofi-utilities"))
hl.bind(mainMod .. " + P", hl.dsp.exec_cmd("hyprlock"))
hl.bind(mainMod .. " + SHIFT + P", hl.dsp.exec_cmd("rofi -m 1 -show power-menu -modi power-menu:~/.config/rofi/modules/rofi-power-menu"))

-- App control
local closeWindowBind = hl.bind(mainMod .. " + Q", hl.dsp.window.close())
hl.bind(mainMod .. " + CTRL + R", hl.dsp.exec_cmd("/home/$USER/.local/scripts/wallbar-reload"))
hl.bind(mainMod .. " + CTRL + Q", hl.dsp.exec_cmd("command -v hyprshutdown >/dev/null 2>&1 && hyprshutdown || hyprctl dispatch 'hl.dsp.exit()'"))

-- Layouts
hl.bind(mainMod .. " + V", hl.dsp.window.float({ action = "toggle" }))
hl.bind(mainMod .. " + TAB", hl.dsp.window.fullscreen({ mode = "maximized", action = "toggle" }))
hl.bind(mainMod .. " + SPACE", hl.dsp.window.fullscreen({ mode = "fullscreen", action = "toggle" }))

-- Switch monitors
hl.bind(mainMod .. " + Z",  hl.dsp.focus({ monitor = "l" }), { desc = "Move workspace to left monitor" })
hl.bind(mainMod .. " + X",  hl.dsp.focus({ monitor = "r" }), { desc = "Move workspace to left monitor" })

-- Move current workspace to monitor
hl.bind(mainMod .. " + SHIFT + Z",  hl.dsp.workspace.move({ monitor = "l" }), { desc = "Move workspace to left monitor" })
hl.bind(mainMod .. " + SHIFT + X", hl.dsp.workspace.move({ monitor = "r" }), { desc = "Move workspace to right monitor" })

-- Move through workspaces
for i = 1, 10 do
  local key = i % 10 -- 10 maps to key 0
  hl.bind(mainMod .. " + " .. key,             hl.dsp.focus({ workspace = i}))
  hl.bind(mainMod .. " + SHIFT + " .. key,     hl.dsp.window.move({ workspace = i }))
end

hl.bind(mainMod .. " + K", hl.dsp.focus({ workspace = "m+1", on_current_monitor }))
hl.bind(mainMod .. " + J", hl.dsp.focus({ workspace = "m-1", on_current_monitor }))
hl.bind(mainMod .. " + mouse_down", hl.dsp.focus({ workspace = "e+1" }))
hl.bind(mainMod .. " + mouse_up",   hl.dsp.focus({ workspace = "e-1" }))

-- Window focus
hl.bind(mainMod .. " + H",  hl.dsp.focus({ direction = "left" }))
hl.bind(mainMod .. " + L", hl.dsp.focus({ direction = "right" }))
hl.bind(mainMod .. " + SHIFT + H", hl.dsp.window.swap({ direction = "l" }), { desc = "Move window left" })
hl.bind(mainMod .. " + SHIFT + L", hl.dsp.window.swap({ direction = "r" }), { desc = "Move window right" })

-- Resize windows
hl.bind(mainMod .. " + mouse:272", hl.dsp.window.drag(),   { mouse = true })
hl.bind(mainMod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })
