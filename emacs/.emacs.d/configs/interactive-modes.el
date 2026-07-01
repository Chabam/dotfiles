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

(defun chbm/set-adwaita-term-colors (&optional dark-mode)
  (let ((adwaita-black "#1d1d20")
        (adwaita-red "#c01c28")
        (adwaita-green "#26a269")
        (adwaita-yellow "#a2734c")
        (adwaita-blue "#12488b")
        (adwaita-magenta "#a347ba")
        (adwaita-cyan "#2aa1b3")
        (adwaita-white "#cfcfcf")
        (adwaita-bright-black "#5d5d5d")
        (adwaita-bright-red "#f66151")
        (adwaita-bright-green "#33d17a")
        (adwaita-bright-yellow "#e9ad0c")
        (adwaita-bright-blue "#2a7bde")
        (adwaita-bright-magenta "#c061cb")
        (adwaita-bright-cyan "#33c7de")
        (adwaita-bright-white "#ffffff"))

    (when dark-mode
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
      (setq adwaita-bright-white   "#F6F5F4"))

    (custom-set-faces
     `(ghostel-color-black ((t (:foreground ,adwaita-black :background ,adwaita-black))))
     `(ghostel-color-red ((t (:foreground ,adwaita-red :background ,adwaita-red))))
     `(ghostel-color-green ((t (:foreground ,adwaita-green :background ,adwaita-green))))
     `(ghostel-color-yellow ((t (:foreground ,adwaita-yellow :background ,adwaita-yellow))))
     `(ghostel-color-blue ((t (:foreground ,adwaita-blue :background ,adwaita-blue))))
     `(ghostel-color-magenta ((t (:foreground ,adwaita-magenta :background ,adwaita-magenta))))
     `(ghostel-color-cyan ((t (:foreground ,adwaita-cyan :background ,adwaita-cyan))))
     `(ghostel-color-white ((t (:foreground ,adwaita-white :background ,adwaita-white))))
     `(ghostel-color-bright-black ((t (:foreground ,adwaita-bright-black :background ,adwaita-bright-black))))
     `(ghostel-color-bright-red ((t (:foreground ,adwaita-bright-red :background ,adwaita-bright-red))))
     `(ghostel-color-bright-green ((t (:foreground ,adwaita-bright-green :background ,adwaita-bright-green))))
     `(ghostel-color-bright-yellow ((t (:foreground ,adwaita-bright-yellow :background ,adwaita-bright-yellow))))
     `(ghostel-color-bright-blue ((t (:foreground ,adwaita-bright-blue :background ,adwaita-bright-blue))))
     `(ghostel-color-bright-magenta ((t (:foreground ,adwaita-bright-magenta :background ,adwaita-bright-magenta))))
     `(ghostel-color-bright-cyan ((t (:foreground ,adwaita-bright-cyan :background ,adwaita-bright-cyan))))
     `(ghostel-color-bright-white ((t (:foreground ,adwaita-bright-white :background ,adwaita-bright-white)))))))

(use-package ghostel
  :ensure t
  :commands (ghostel)
  :hook ((auto-dark-dark-mode . (lambda () (chbm/set-adwaita-term-colors t)))
         (auto-dark-light-mode . chbm/set-adwaita-term-colors)))

(provide 'interactive-modes)
