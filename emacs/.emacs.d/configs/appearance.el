;; Removing GUI features
(setq default-frame-alist '((fullscreen . maximized)
			    (menu-bar-lines . 0)
			    (tool-bar-lines . 0)
			    (vertical-scroll-bars)
			    (horizontal-scroll-bars)))

(cond ((find-font (font-spec :name "Iosevka"))
       (add-to-list 'default-frame-alist '(font . "Iosevka-12")))
      ((find-font (font-spec :name "Adwaita Mono"))
       (add-to-list 'default-frame-alist '(font . "Adwaita Mono-12"))))


(setq default-frame-scroll-bars 'right)
(setq scroll-bar-mode nil)

(add-hook 'after-init-hook #'pixel-scroll-mode)

(setq diff-font-lock-prettify t)

;; Enables faster scrolling. This may result in brief periods of inaccurate
;; syntax highlighting, which should quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Disable fontification during user input to reduce lag in large buffers.
;; Also helps marginally with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)

;; Tab bar options
(setq tab-bar-close-button-show nil)
(setq tab-bar-new-button-show nil)
(setq tab-bar-show 1)

(use-package modus-themes
  :ensure t
  :init
  (custom-set-faces
   '(variable-pitch ((t (:family "Iosevka")))))

  (setq modus-themes-common-palette-overrides
      `(
        (cursor magenta-warmer)
        (bg-region bg-lavender)
        (fg-region unspecified)

        (string green-cooler)
        (bg-changed-fringe bg-cyan-intense)

        ;; Blue modeline
        (bg-mode-line-active bg-blue-intense)
        (fg-mode-line-active fg-main)

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

(setq modus-themes-variable-pitch-ui nil
      modus-themes-italic-constructs nil
      modus-themes-bold-constructs t
      modus-themes-mixed-fonts nil))

(use-package auto-dark
  :ensure t
  :defer t
  :hook (after-init . auto-dark-mode)
  :custom
  (auto-dark-themes '((modus-vivendi) (modus-operandi))))

;; Thanks Prot!
(defun prot-spell-ispell-display-buffer (buffer)
  "Function to override `ispell-display-buffer' for BUFFER.
Use this as `advice-add' to override the aforementioned Ispell
function.  Then you can control the buffer's specifics via
`display-buffer-alist' (how it ought to be!)."
  (pop-to-buffer buffer)
  (set-window-point (get-buffer-window buffer) (point-min)))

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

;; Whitespace
(setq whitespace-style '(face indentation tabs tab-mark spaces space-mark
                              newline newline-mark trailing))


(provide 'appearance)
