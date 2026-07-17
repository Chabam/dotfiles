(use-package comint
  :ensure nil
  :hook
  (comint-output-filter-functions . comint-osc-process-output)
  :config
  (setq ansi-color-for-comint-mode t)
  (setq comint-buffer-maximum-size (* 4 1024))
  (setq comint-prompt-read-only t)
  (setq comint-completion-autolist t)
  (setq comint-input-ignoredups t)
  (setq-default comint-scroll-to-bottom-on-input t)
  (setq-default comint-scroll-to-bottom-on-output nil)
  (setq-default comint-input-autoexpand 'input))

(use-package eshell
  :ensure nil
  :commands (eshell)
  :hook ((eshell-hist-mode . (lambda ()
                               (define-key eshell-hist-mode-map (kbd "C-<up>") nil)
                               (define-key eshell-hist-mode-map (kbd "C-<down>") nil))))
  :config
  (setq eshell-prompt-function
        (lambda ()
          (modus-themes-with-colors
            (concat (propertize (abbreviate-file-name (eshell/pwd)) 'face `(:foreground ,cyan))
                    (unless (eshell-exit-success-p)
                      (propertize (format " [%d]" eshell-last-command-status)
                                  'face `(:foreground ,red)))
                    "\n"
                    (if (= (file-user-uid) 0) "# " "$ ")))))
  (add-hook 'eshell-preoutput-filter-functions  #'ansi-color-apply))

(use-package em-hist
  :ensure nil
  :bind (:map eshell-hist-mode-map
              ("M-r" . consult-history)
              ("M-s" . consult-history)))

(provide 'interactive-modes)
