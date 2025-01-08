-- Pull in the wezterm API
local wezterm = require("wezterm")

-- This will hold the configuration.
local config = wezterm.config_builder()

config.hide_tab_bar_if_only_one_tab = true
config.term = "xterm-256color"
config.font = wezterm.font("Iosevka Nerd Font Mono")

-- This is where you actually apply your config choices
local light = {
	foreground = "#1e1e1e",
	background = "#ffffff",
	cursor_bg = "#1e1e1e",
	cursor_fg = "#ffffff",
	cursor_border = "#1e1e1e",
	ansi = {
		"#1e1e1e",
		"#c01c28",
		"#26a269",
		"#a2734c",
		"#12488b",
		"#a347ba",
		"#2aa1b3",
		"#cfcfcf",
	},
	brights = {
		"#5d5d5d",
		"#f66151",
		"#33d17a",
		"#e9ad0c",
		"#2a7bde",
		"#c061cb",
		"#33c7de",
		"#ffffff",
	},
}

local dark = {
	foreground = "#cfcfcf",
	background = "#1e1e1e",
	cursor_bg = "#cfcfcf",
	cursor_fg = "#1e1e1e",
	cursor_border = "#cfcfcf",
	ansi = {
		"#1e1e1e",
		"#c01c28",
		"#26a269",
		"#a2734c",
		"#12488b",
		"#a347ba",
		"#2aa1b3",
		"#cfcfcf",
	},
	brights = {
		"#5d5d5d",
		"#f66151",
		"#33d17a",
		"#e9ad0c",
		"#2a7bde",
		"#c061cb",
		"#33c7de",
		"#ffffff",
	},
}
-- For example, changing the color scheme:
function get_appearance()
	if wezterm.gui then
		return wezterm.gui.get_appearance()
	end
	return dark
end

function scheme_for_appearance(appearance)
	if appearance:find("Dark") then
		return dark
	else
		return light
	end
end
config.colors = scheme_for_appearance(get_appearance())

config.enable_wayland = false
-- and finally, return the configuration to wezterm
return config
