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

(use-package eat
  :config
  (setq adwaita-black          "#241F31")
  (setq adwaita-red            "#C01C28")
  (setq adwaita-green          "#2EC27E")
  (setq adwaita-yellow         "#F5C211")
  (setq adwaita-blue           "#1E78E4")
  (setq adwaita-magenta        "#9841BB")
  (setq adwaita-cyan           "#0AB9DC")
  (setq adwaita-grey           "#C0BFBC")
  (setq adwaita-bright-black   "#5E5C64")
  (setq adwaita-bright-red     "#ED333B")
  (setq adwaita-bright-green   "#57E389")
  (setq adwaita-bright-yellow  "#F8E45C")
  (setq adwaita-bright-blue    "#51A1FF")
  (setq adwaita-bright-magenta "#C061CB")
  (setq adwaita-bright-cyan    "#4FD2FD")
  (setq adwaita-bright-grey    "#F6F5F4")

  (set-face-attribute 'eat-term-color-0 nil :foreground adwaita-black :background adwaita-black)
  (set-face-attribute 'eat-term-color-1 nil :foreground adwaita-red :background adwaita-red)
  (set-face-attribute 'eat-term-color-2 nil :foreground adwaita-green :background adwaita-green)
  (set-face-attribute 'eat-term-color-3 nil :foreground adwaita-yellow :background adwaita-yellow)
  (set-face-attribute 'eat-term-color-4 nil :foreground adwaita-blue :background adwaita-blue)
  (set-face-attribute 'eat-term-color-5 nil :foreground adwaita-magenta :background adwaita-magenta)
  (set-face-attribute 'eat-term-color-6 nil :foreground adwaita-cyan :background adwaita-cyan)
  (set-face-attribute 'eat-term-color-7 nil :foreground adwaita-grey :background adwaita-grey)
  (set-face-attribute 'eat-term-color-8 nil :foreground adwaita-bright-black :background adwaita-bright-black)
  (set-face-attribute 'eat-term-color-9 nil :foreground adwaita-bright-red :background adwaita-bright-red)
  (set-face-attribute 'eat-term-color-10 nil :foreground adwaita-bright-green :background adwaita-bright-green)
  (set-face-attribute 'eat-term-color-11 nil :foreground adwaita-bright-yellow :background adwaita-bright-yellow)
  (set-face-attribute 'eat-term-color-12 nil :foreground adwaita-bright-blue :background adwaita-bright-blue)
  (set-face-attribute 'eat-term-color-13 nil :foreground adwaita-bright-magenta :background adwaita-bright-magenta)
  (set-face-attribute 'eat-term-color-14 nil :foreground adwaita-bright-cyan :background adwaita-bright-cyan)
  (set-face-attribute 'eat-term-color-15 nil :foreground adwaita-bright-grey :background adwaita-bright-grey))
