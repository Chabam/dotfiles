# Mouse support
set -g mouse on

# Set the default terminal mode to 256color mode
set -g default-terminal "screen-256color"

set -g status-left '#[fg=colour235,bg=colour252,bold] ❐ #S #[fg=colour252,bg=colour238,nobold]⮀#[fg=colour245,bg=colour238,bold] #(whoami) #[fg=colour238,bg=colour234,nobold]⮀'
set -g status-right '#[fg=colour238,bg=colour234,nobold]⮂#[fg=colour245,bg=colour238] #h #[fg=colour252]⮂#[fg=colour235,bg=colour252,bold] #(wemux status_users) '
set -g window-status-format "#[fg=white,bg=colour234] #I #W "
set -g window-status-current-format "#[fg=colour234,bg=colour39]⮀#[fg=colour25,bg=colour39,noreverse,bold] #I ⮁ #W #[fg=colour39,bg=colour234,nobold]⮀"
