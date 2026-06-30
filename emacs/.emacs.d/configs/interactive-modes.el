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

(defun eshell/clear () (eshell/clear-scrollback))

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

(use-package ghostel
  :ensure t
  :commands (ghostel)
  :config
  (setq adwaita-black          "#241F31")
  (setq adwaita-red            "#C01C28")
  (setq adwaita-green          "#2EC27E")
  (setq adwaita-yellow         "#F5C211")
  (setq adwaita-blue           "#1E78E4")
  (setq adwaita-magenta        "#9841BB")
  (setq adwaita-cyan           "#0AB9DC")
  (setq adwaita-white          "#C0BFBC")
  (setq adwaita-bright-black   "#5E5C64")
  (setq adwaita-bright-red     "#ED333B")
  (setq adwaita-bright-green   "#57E389")
  (setq adwaita-bright-yellow  "#F8E45C")
  (setq adwaita-bright-blue    "#51A1FF")
  (setq adwaita-bright-magenta "#C061CB")
  (setq adwaita-bright-cyan    "#4FD2FD")
  (setq adwaita-bright-white   "#F6F5F4")

  (set-face-attribute 'ghostel-color-black nil :foreground adwaita-black :background adwaita-black)
  (set-face-attribute 'ghostel-color-red nil :foreground adwaita-red :background adwaita-red)
  (set-face-attribute 'ghostel-color-green nil :foreground adwaita-green :background adwaita-green)
  (set-face-attribute 'ghostel-color-yellow nil :foreground adwaita-yellow :background adwaita-yellow)
  (set-face-attribute 'ghostel-color-blue nil :foreground adwaita-blue :background adwaita-blue)
  (set-face-attribute 'ghostel-color-magenta nil :foreground adwaita-magenta :background adwaita-magenta)
  (set-face-attribute 'ghostel-color-cyan nil :foreground adwaita-cyan :background adwaita-cyan)
  (set-face-attribute 'ghostel-color-white nil :foreground adwaita-white :background adwaita-white)
  (set-face-attribute 'ghostel-color-bright-black nil :foreground adwaita-bright-black :background adwaita-bright-black)
  (set-face-attribute 'ghostel-color-bright-red nil :foreground adwaita-bright-red :background adwaita-bright-red)
  (set-face-attribute 'ghostel-color-bright-green nil :foreground adwaita-bright-green :background adwaita-bright-green)
  (set-face-attribute 'ghostel-color-bright-yellow nil :foreground adwaita-bright-yellow :background adwaita-bright-yellow)
  (set-face-attribute 'ghostel-color-bright-blue nil :foreground adwaita-bright-blue :background adwaita-bright-blue)
  (set-face-attribute 'ghostel-color-bright-magenta nil :foreground adwaita-bright-magenta :background adwaita-bright-magenta)
  (set-face-attribute 'ghostel-color-bright-cyan nil :foreground adwaita-bright-cyan :background adwaita-bright-cyan)
  (set-face-attribute 'ghostel-color-bright-white nil :foreground adwaita-bright-white :background adwaita-bright-white))

(provide 'interactive-modes)
