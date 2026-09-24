---@meta
-------------------------------------------------------------------------------
-- OXWM Configuration File
-------------------------------------------------------------------------------
-- This is the default configuration for OXWM, a dynamic window manager.
-- Edit this file and reload with Mod+Shift+R (no compilation needed)
--
-- For more information about configuring OXWM, see the documentation.
-- The Lua Language Server provides autocomplete and type checking.
-------------------------------------------------------------------------------

---Load type definitions for LSP
---@module 'oxwm'

-------------------------------------------------------------------------------
-- Variables
-------------------------------------------------------------------------------
-- Define your variables here for easy customization throughout the config.
-- This makes it simple to change keybindings, colors, and settings in one place.

-- Modifier key: "Mod4" is the Super/Windows key, "Mod1" is Alt
local modkey = "Mod4"

-- Terminal emulator command (defaults to alacritty)
local terminal = "ghostty"

-- Color palette - customize these to match your theme
-- Alternatively you can import other files in here, such as
-- local colors = require("colors.lua") and make colors.lua a file
-- in the ~/.config/oxwm directory
local colors = {
	fg = "#c0caf5",
	red = "#f7768e",
	bg = "#1a1b26",
	cyan = "#0db9d7",
	green = "#9ece6a",
	lavender = "#a9b1d6",
	light_blue = "#7aa2f7",
	grey = "#bbbbbb",
	blue = "#6dade3",
	purple = "#ad8ee6",
}

-- Workspace tags - can be numbers, names, or icons (requires a Nerd Font)
local tags = { "1", "2", "3", "4", "5", "6", "7", "8", "9" }
-- local tags = { "", "󰊯", "", "", "󰙯", "󱇤", "", "󱘶", "󰧮" } -- Example of nerd font icon tags

-- Font for the status bar (use "fc-list" to see available fonts)
local bar_font = "JetBrainsMono Nerd Font:style=Bold:size=10"

-- Define your blocks
-- Similar to widgets in qtile, or dwmblocks
local blocks = {
	oxwm.bar.block.ram({
		format = "Ram: {used}/{total} GB",
		interval = 5,
		color = colors.light_blue,
		underline = true,
	}),
	oxwm.bar.block.static({
		text = "│",
		interval = 999999999,
		color = colors.lavender,
		underline = false,
	}),
	oxwm.bar.block.datetime({
		format = "{}",
		date_format = "%a, %b %d - %-I:%M %P",
		interval = 1,
		color = colors.cyan,
		underline = true,
	}),
	oxwm.bar.block.static({
		text = "│",
		interval = 999999999,
		color = colors.lavender,
		underline = false,
	}),
	oxwm.bar.block.battery({
		format = "Bat: {}%",
		charging = "⚡ Bat: {}%",
		discharging = "- Bat: {}%",
		full = "✓ Bat: {}%",
		interval = 30,
		color = colors.green,
		underline = true,
		-- click: run a command when the block is clicked
		-- click = "alacritty -e btop",
		-- click = { command = "bluetui", floating = true },
	}),
}

-------------------------------------------------------------------------------
-- Basic Settings
-------------------------------------------------------------------------------
oxwm.set_terminal(terminal)
oxwm.set_modkey(modkey) -- This is for Mod + mouse binds, such as drag/resize
oxwm.set_tags(tags)

-- Set default layout (tiling by default)
-- oxwm.set_layout("tiling")

-------------------------------------------------------------------------------
-- Layouts
-------------------------------------------------------------------------------
-- Set custom symbols for layouts (displayed in the status bar)
-- Available layouts: "tiling", "normie" (floating), "grid", "monocle", "tabbed", "dwindle"
oxwm.set_layout_symbol("tiling", "[T]")
oxwm.set_layout_symbol("normie", "[F]")
oxwm.set_layout_symbol("tabbed", "[=]")
-- oxwm.set_layout_symbol("dwindle", "[\\]")

-- Example: bind dwindle (fibonacci) layout
-- oxwm.key.bind({ modkey }, "R", oxwm.layout.set("dwindle"))

-- Set default layout of specific tag (tag_index, layout_name)
-- Unset value uses oxwm.set_layout value
-- oxwm.set_tag_layout(1, "grid")

-------------------------------------------------------------------------------
-- Appearance
-------------------------------------------------------------------------------
-- Border configuration

-- Width in pixels
oxwm.border.set_width(2)
-- Color of focused window border
oxwm.border.set_focused_color(colors.blue)
-- Color of unfocused window borders
oxwm.border.set_unfocused_color(colors.grey)

-- Where floating windows spawn: "top-left", "top-center", "top-right",
-- "center-left", "center", "center-right", "bottom-left", "bottom-center", "bottom-right"
oxwm.set_floating_position("center")

-- Smart Enabled = No border if 1 window
oxwm.gaps.set_smart(enabled)
-- Inner gaps (horizontal, vertical) in pixels
oxwm.gaps.set_inner(5, 5)
-- Outer gaps (horizontal, vertical) in pixels
oxwm.gaps.set_outer(5, 5)

-------------------------------------------------------------------------------
-- Window Rules
-------------------------------------------------------------------------------
-- Rules allow you to automatically configure windows based on their properties
-- You can match windows by class, instance, title, or role
-- Available properties: floating, tag, fullscreen, etc.
--
-- Common use cases:
-- - Force floating for certain applications (dialogs, utilities)
-- - Send specific applications to specific workspaces
-- - Configure window behavior based on title or class

-- Examples (uncomment to use):
oxwm.rule.add({ class = "ferdium", tag = 1 })
oxwm.rule.add({ class = "app.zen_browser.zen", tag = 2 })
oxwm.rule.add({ class = "brave-origin", tag = 2 })
oxwm.rule.add({ class = "com.mitchellh.ghostty", tag = 4 })
oxwm.rule.add({ class = "pcmanfm", tag = 5 })
oxwm.rule.add({ class = "rawtherapee", tag = 1 })
oxwm.rule.add({ class = "Vial", tag = 6 })
oxwm.rule.add({ class = "zmk-studio", tag = 6 })
oxwm.rule.add({ class = "calibre-gui", tag = 6 })
oxwm.rule.add({ class = "nwg-look", tag = 6 })
oxwm.rule.add({ class = "org.qbittorrent.qBittorrent", tag = 6 })
oxwm.rule.add({ class = "NexusMods.App", tag = 6 })
oxwm.rule.add({ class = "steam", tag = 7 })
oxwm.rule.add({ class = "heroic", tag = 7 })
oxwm.rule.add({ class = "net.lutris.Lutris", tag = 7 })
oxwm.rule.add({ class = "com.github.wwmm.easyeffects", tag = 8 })
oxwm.rule.add({ class = "mpv", tag = 9 })

-- To find window properties, use xprop and click on the window
-- WM_CLASS(STRING) shows both instance and class (instance, class)

-------------------------------------------------------------------------------
-- Status Bar Configuration
-------------------------------------------------------------------------------
-- Font configuration
oxwm.bar.set_font(bar_font)

-- Position configuration (top/bottom, top is default)
-- oxwm.bar.set_position("bottom")

-- Set your blocks here (defined above)
oxwm.bar.set_blocks(blocks)

-- Bar color schemes (for workspace tag display)
-- Parameters: foreground, background, border

-- Unoccupied tags
oxwm.bar.set_scheme_normal(colors.fg, colors.bg, "#444444")
-- Occupied tags
oxwm.bar.set_scheme_occupied(colors.cyan, colors.bg, colors.cyan)
-- Currently selected tag
oxwm.bar.set_scheme_selected(colors.cyan, colors.bg, colors.purple)
-- Urgent tags (windows requesting attention)
oxwm.bar.set_scheme_urgent(colors.red, colors.bg, colors.red)

-- Hide tags that have no windows and are not selected
-- oxwm.bar.set_hide_vacant_tags(true)

-------------------------------------------------------------------------------
-- Keybindings
-------------------------------------------------------------------------------
-- Keybindings are defined using oxwm.key.bind(modifiers, key, action)
-- Modifiers: {"Mod4"}, {"Mod1"}, {"Shift"}, {"Control"}, or combinations like {"Mod4", "Shift"}
-- Keys: Use uppercase for letters (e.g., "Return", "H", "J", "K", "L")
-- Actions: Functions that return actions (e.g., oxwm.spawn(), oxwm.client.kill())
--
-- A list of available keysyms can be found in the X11 keysym definitions.
-- Common keys: Return, Space, Tab, Escape, Backspace, Delete, Left, Right, Up, Down

-- Apps
oxwm.key.bind({ modkey }, "Return", oxwm.spawn_terminal())
oxwm.key.bind({ modkey }, "S", oxwm.spawn({ "sh", "-c", "rofi -show drun" }))
oxwm.key.bind({ modkey }, "W", oxwm.spawn("app.zen_browser.zen"))
oxwm.key.bind({ modkey, "Shift" }, "W", oxwm.spawn("zen_browser.zen --private-window"))
oxwm.key.bind({ modkey }, "D", oxwm.spawn("ghostty -e nvim"))
-- oxwm.key.bind({ modkey }, "D", oxwm.spawn("emacsclient -c -a 'emacs'"))
oxwm.key.bind({ modkey }, "A", oxwm.spawn("ghostty -e yazi"))
-- oxwm.key.bind({ modkey }, "A", oxwm.spawn("emacsclient -c -a 'emacs' --eval '(dired nil)'"))
oxwm.key.bind({ modkey, "Shift" }, "A", oxwm.spawn("pcmanfm"))
oxwm.key.bind({ modkey }, "U", oxwm.spawn("sh", "-c", "/home/$USER/Projekty/scripts/rofi-utilities"))
oxwm.key.bind(
	{ modkey },
	"P",
	oxwm.spawn({ "sh", "-c", "rofi -m 1 -show power-menu -modi power-menu:~/.config/rofi/modules/rofi-power-menu" })
)

-- App Control
oxwm.key.bind({ modkey }, "Q", oxwm.client.kill())
oxwm.key.bind({ modkey, "Control" }, "Q", oxwm.quit())
oxwm.key.bind({ modkey, "Control" }, "R", oxwm.restart())

-- Keybind overlay - Shows important keybindings on screen
oxwm.key.bind({ modkey, "Shift" }, "Slash", oxwm.show_keybinds())

-- Window state toggles
oxwm.key.bind({ modkey }, "Space", oxwm.client.toggle_fullscreen())
oxwm.key.bind({ modkey }, "V", oxwm.client.toggle_floating())

-- Cycle through layouts
oxwm.key.bind({ modkey }, "Tab", oxwm.layout.cycle())

-- Decrease/Increase master area width
oxwm.key.bind({ modkey, "Control" }, "H", oxwm.set_master_factor(-5))
oxwm.key.bind({ modkey, "Control" }, "L", oxwm.set_master_factor(5))
-- Enable tiled resize mode: Mod+RMB drag adjusts mfact instead of floating
-- oxwm.tiled_resize_mode(true)

-- Focus movement [1 for up in the stack, -1 for down]
oxwm.key.bind({ modkey }, "H", oxwm.client.focus_stack(1))
oxwm.key.bind({ modkey }, "L", oxwm.client.focus_stack(-1))

-- Window movement (swap position in stack)
oxwm.key.bind({ modkey, "Shift" }, "H", oxwm.client.move_stack(1))
oxwm.key.bind({ modkey, "Shift" }, "L", oxwm.client.move_stack(-1))

-- Focus next/previous Monitors
oxwm.key.bind({ modkey }, "Z", oxwm.monitor.focus(-1))
oxwm.key.bind({ modkey }, "X", oxwm.monitor.focus(1))
-- Move window to next/previous Monitors
oxwm.key.bind({ modkey, "Shift" }, "Z", oxwm.monitor.tag(-1))
oxwm.key.bind({ modkey, "Shift" }, "X", oxwm.monitor.tag(1))

-- Workspace (tag) navigation
-- Switch to workspace N (tags are 0-indexed, so tag "1" is index 0)
oxwm.key.bind({ modkey }, "1", oxwm.tag.view(0))
oxwm.key.bind({ modkey }, "2", oxwm.tag.view(1))
oxwm.key.bind({ modkey }, "3", oxwm.tag.view(2))
oxwm.key.bind({ modkey }, "4", oxwm.tag.view(3))
oxwm.key.bind({ modkey }, "5", oxwm.tag.view(4))
oxwm.key.bind({ modkey }, "6", oxwm.tag.view(5))
oxwm.key.bind({ modkey }, "7", oxwm.tag.view(6))
oxwm.key.bind({ modkey }, "8", oxwm.tag.view(7))
oxwm.key.bind({ modkey }, "9", oxwm.tag.view(8))

-- Move focused window to workspace N
oxwm.key.bind({ modkey, "Shift" }, "1", oxwm.tag.move_to(0))
oxwm.key.bind({ modkey, "Shift" }, "2", oxwm.tag.move_to(1))
oxwm.key.bind({ modkey, "Shift" }, "3", oxwm.tag.move_to(2))
oxwm.key.bind({ modkey, "Shift" }, "4", oxwm.tag.move_to(3))
oxwm.key.bind({ modkey, "Shift" }, "5", oxwm.tag.move_to(4))
oxwm.key.bind({ modkey, "Shift" }, "6", oxwm.tag.move_to(5))
oxwm.key.bind({ modkey, "Shift" }, "7", oxwm.tag.move_to(6))
oxwm.key.bind({ modkey, "Shift" }, "8", oxwm.tag.move_to(7))
oxwm.key.bind({ modkey, "Shift" }, "9", oxwm.tag.move_to(8))

-- Combo view (view multiple tags at once) {argos_nothing}
-- Example: Mod+Ctrl+2 while on tag 1 will show BOTH tags 1 and 2
oxwm.key.bind({ modkey, "Control" }, "1", oxwm.tag.toggleview(0))
oxwm.key.bind({ modkey, "Control" }, "2", oxwm.tag.toggleview(1))
oxwm.key.bind({ modkey, "Control" }, "3", oxwm.tag.toggleview(2))
oxwm.key.bind({ modkey, "Control" }, "4", oxwm.tag.toggleview(3))
oxwm.key.bind({ modkey, "Control" }, "5", oxwm.tag.toggleview(4))
oxwm.key.bind({ modkey, "Control" }, "6", oxwm.tag.toggleview(5))
oxwm.key.bind({ modkey, "Control" }, "7", oxwm.tag.toggleview(6))
oxwm.key.bind({ modkey, "Control" }, "8", oxwm.tag.toggleview(7))
oxwm.key.bind({ modkey, "Control" }, "9", oxwm.tag.toggleview(8))

-- Multi tag (window on multiple tags)
-- Example: Mod+Ctrl+Shift+2 puts focused window on BOTH current tag and tag 2
oxwm.key.bind({ modkey, "Control", "Shift" }, "1", oxwm.tag.toggletag(0))
oxwm.key.bind({ modkey, "Control", "Shift" }, "2", oxwm.tag.toggletag(1))
oxwm.key.bind({ modkey, "Control", "Shift" }, "3", oxwm.tag.toggletag(2))
oxwm.key.bind({ modkey, "Control", "Shift" }, "4", oxwm.tag.toggletag(3))
oxwm.key.bind({ modkey, "Control", "Shift" }, "5", oxwm.tag.toggletag(4))
oxwm.key.bind({ modkey, "Control", "Shift" }, "6", oxwm.tag.toggletag(5))
oxwm.key.bind({ modkey, "Control", "Shift" }, "7", oxwm.tag.toggletag(6))
oxwm.key.bind({ modkey, "Control", "Shift" }, "8", oxwm.tag.toggletag(7))
oxwm.key.bind({ modkey, "Control", "Shift" }, "9", oxwm.tag.toggletag(8))

-------------------------------------------------------------------------------
-- Advanced: Keychords
-------------------------------------------------------------------------------
-- Keychords allow you to bind multiple-key sequences (like Emacs or Vim)
-- Format: {{modifiers}, key1}, {{modifiers}, key2}, ...
-- Example: Press Mod4+Space, then release and press T to spawn a terminal
oxwm.key.chord({
	{ { modkey }, "Y" },
	{ {}, "G" },
}, oxwm.toggle_gaps())

oxwm.key.chord({
	{ { modkey }, "Y" },
	{ {}, "B" },
}, oxwm.toggle_bar())

-------------------------------------------------------------------------------
-- Autostart
-------------------------------------------------------------------------------
-- Commands to run once when OXWM starts
-- Uncomment and modify these examples, or add your own

oxwm.autostart("xrandr --output eDP-2 --off --output HDMI-1-0 --mode 1920x1080 --rate 144")
oxwm.autostart("picom --config ~/.config/picom/picom.conf")
oxwm.autostart("feh --bg-scale ~/Obrazy/wallpapers/tokyonight/tokyonight_1.png")
oxwm.autostart("brightnessctl --device=intel_backlight set 40%")
oxwm.autostart("easyeffects --gapplication-service &")
