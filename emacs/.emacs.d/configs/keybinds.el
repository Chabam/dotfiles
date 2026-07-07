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

(defun chbm/copy-pwd (&optional abrev)
  (interactive "P")
  (kill-new (if abrev
                (abbreviate-file-name default-directory)
              default-directory)))

; Removing some default binds
;; minimize
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-z") nil)
;; "hello" buffer
(global-set-key (kbd "C-h h") nil)
;; Closing emacs 🙂 (actually gets rebound later)
(global-set-key (kbd "C-x C-c") nil)

;; rebinds
(global-set-key (kbd "C-x M-g") 'grep)
(global-set-key (kbd "C-x M-e") 'eshell)
(global-set-key (kbd "C-x C-c C-c") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Enabling some disabled commands
(put 'narrow-to-region 'disabled nil)

(provide 'keybinds)
