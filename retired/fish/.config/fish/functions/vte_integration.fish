function send_vte_termprop_signal
    printf \e\]666\;%s!\e\\ $argv
end

function notify_vte_preexec --on-event fish_preexec
    send_vte_termprop_signal vte.shell.preexec
end

function notify_vte_precmd --on-event fish_postexec
    send_vte_termprop_signal vte.shell.precmd
end
