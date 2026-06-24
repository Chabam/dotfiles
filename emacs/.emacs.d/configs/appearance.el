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
      (set-face-attribute 'default nil :family "Iosevka" :height font-size)
      (set-face-attribute 'fixed-pitch nil :family "Iosevka")
      (set-face-attribute 'variable-pitch nil :family "Iosevka")
      (set-face-attribute 'tab-bar nil :height font-size)
      (set-face-attribute 'tab-bar-tab nil :height font-size)
      (set-face-attribute 'tab-bar-tab-inactive nil :height font-size))))

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

(provide 'appearance)
