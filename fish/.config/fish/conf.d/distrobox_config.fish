if test -e /etc/fish/conf.d/distrobox_config.fish
	source /etc/fish/conf.d/distrobox_config.fish
	functions -e fish_prompt
	source $XDG_CONFIG_HOME/fish/functions/fish_prompt.fish
end
