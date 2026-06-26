(use-package which-key
  :ensure nil
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-max-display-columns 3)
  (setq which-key-add-column-padding 1)
  (setq which-key-max-description-length nil)
  (setq which-key-prefix-prefix "... ")
  (setq which-key-separator "  "))

(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode)
  :config
  (setq repeat-on-final-keystroke t
        repeat-exit-timeout 5
        repeat-exit-key "<escape>"
        repeat-keep-prefix nil
        repeat-check-key t
        set-mark-command-repeat-pop t))

(provide 'keybinds)
