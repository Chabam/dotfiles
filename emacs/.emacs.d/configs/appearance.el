;; Removing GUI features
(setq default-frame-alist '((fullscreen . maximized)
			    (menu-bar-lines . 0)
			    (tool-bar-lines . 0)
			    (vertical-scroll-bars)
			    (horizontal-scroll-bars)))


(setq default-frame-scroll-bars 'right)
(setq scroll-bar-mode nil)

(defun chbm/set-fonts (&optional font-size &rest _)
  "Set fonts for frame and after theme"
  (interactive (list (setq font-size (read-number "Font size: " 120))))
  (when (display-graphic-p)
    (let ((font-size (or font-size 120)))
      (custom-set-faces
       `(default ((t (:family "Iosevka" :height ,font-size))))
       `(fixed-pitch ((t (:family "Iosevka"))))
       `(variable-pitch ((t (:family "Iosevka"))))
       `(tab-bar ((t (:height ,font-size))))
       `(tab-bar-tab ((t (:height ,font-size))))
       `(tab-bar-tab-inactive ((t (:height ,font-size))))))))

(chbm/set-fonts)

(require-theme 'modus-themes)

(setq modus-themes-common-palette-overrides
      `(
        (cursor magenta-warmer)
        (bg-region bg-lavender)
        (fg-region unspecified)

        (string green-cooler)
        (bg-changed-fringe bg-cyan-intense)

        ;; Blue modeline
        ;; (bg-mode-line-active bg-blue-intense)
        ;; (fg-mode-line-active fg-main)

        ;; Number line invisible
        (fringe unspecified)
        (fg-line-number-inactive fg-dim)
        (fg-line-number-active cursor)
        (bg-line-number-inactive unspecified)
        (bg-line-number-active unspecified)

        ;; Org mode
        (fg-heading-0 blue-cooler)
        (fg-heading-1 magenta-cooler)
        (fg-heading-2 magenta-faint)
        (fg-heading-3 blue)
        (fg-heading-4 cyan)
        (fg-heading-5 green-warmer)
        (fg-heading-6 yellow)
        (fg-heading-7 red)
        (fg-heading-8 magenta)

        ;; Org agenda
        (date-common cyan)
        (date-deadline red-warmer)
        (date-event magenta-warmer)
        (date-holiday blue)
        (date-now yellow-warmer)
        (date-scheduled magenta-cooler)
        (date-weekday cyan-cooler)
        (date-weekend blue-faint)))

(setq modus-themes-variable-pitch-ui t
      modus-themes-italic-constructs nil
      modus-themes-bold-constructs t
      modus-themes-mixed-fonts t)

(use-package auto-dark
  :ensure t
  :defer t
  :init (auto-dark-mode)
  :custom
  (auto-dark-themes '((modus-vivendi) (modus-operandi))))

(advice-add #'ispell-display-buffer :override #'prot-spell-ispell-display-buffer)

(setq window-combination-resize t)
(setq even-window-sizes 'height-only)
(setq window-sides-vertical nil)
(setq switch-to-buffer-in-dedicated-window 'pop)
(setq split-height-threshold 85)
(setq split-width-threshold 125)
(setq window-min-height 3)
(setq window-min-width 30)
(setq fit-window-to-buffer-horizontally t)

(setq display-buffer-alist
      '(;; Agenda at bottom
        ("\\*\\(Org \\(Select\\|Note\\)\\|Agenda Commands\\)\\*"
         (display-buffer-in-side-window)
         (side . bottom)
         (slot . 0)
         (window-parameters . ((mode-line-format . none))))
        ;; Embark at bottom
        ("\\*Embark Actions\\*"
         (display-buffer-below-selected)
         (window-height . fit-window-to-buffer)
         (window-parameters . ((no-other-window . t)
                               (mode-line-format . none))))
        ;; ispell-word at bottom
        ("\\*Choices\\*"
         (display-buffer-below-selected)
         (window-height . fit-window-to-buffer))
        ))


(provide 'appearance)
