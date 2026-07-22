;; Removing GUI features
(setq default-frame-alist '((fullscreen . maximized)
			    (menu-bar-lines . 0)
			    (tool-bar-lines . 0)
			    (vertical-scroll-bars)
			    (horizontal-scroll-bars)
                (font . "Jetbrains Mono-12")))

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

(use-package rainbow-mode
  :ensure nil
  :commands (rainbow-mode)
  :config
  (setq rainbow-x-colors nil))

(use-package modus-themes
  :ensure t
  :init
  (custom-set-faces
   '(variable-pitch ((t (:family "Jetbrains Mono")))))

  ;; From https://gitlab.gnome.org/chergert/ptyxis/-/blob/cbdee77429b8a4b70f1bea95af559eeddbb4ee17/src/palettes/gnome.palette
  (setq modus-vivendi-palette-overrides
        '((fg-term-black          "#241F31")
          (bg-term-black          fg-term-black)
          (fg-term-red            "#C01C28")
          (bg-term-red            fg-term-red)
          (fg-term-green          "#2EC27E")
          (bg-term-green          fg-term-green)
          (fg-term-yellow         "#F5C211")
          (bg-term-yellow         fg-term-yellow)
          (fg-term-blue           "#1E78E4")
          (bg-term-blue           fg-term-blue)
          (fg-term-magenta        "#9841BB")
          (bg-term-magenta        fg-term-magenta)
          (fg-term-cyan           "#0AB9DC")
          (bg-term-cyan           fg-term-cyan)
          (fg-term-white          "#C0BFBC")
          (bg-term-white          fg-term-white)
          (fg-term-black-bright   "#5E5C64")
          (bg-term-black-bright   fg-term-black-bright)
          (fg-term-red-bright     "#ED333B")
          (bg-term-red-bright     fg-term-red-bright)
          (fg-term-green-bright   "#57E389")
          (bg-term-green-bright   fg-term-green-bright)
          (fg-term-yellow-bright  "#F8E45C")
          (bg-term-yellow-bright  fg-term-yellow-bright)
          (fg-term-blue-bright    "#51A1FF")
          (bg-term-blue-bright    fg-term-blue-bright)
          (fg-term-magenta-bright "#C061CB")
          (bg-term-magenta-bright fg-term-magenta-bright)
          (fg-term-cyan-bright    "#4FD2FD")
          (bg-term-cyan-bright    fg-term-cyan-bright)
          (fg-term-white-bright   "#F6F5F4")
          (bg-term-white-bright   fg-term-white-bright)))

  (setq modus-operandi-palette-overrides
        '((fg-term-black          "#1d1d20")
          (bg-term-black          fg-term-black)
          (fg-term-red            "#c01c28")
          (bg-term-red            fg-term-red)
          (fg-term-green          "#26a269")
          (bg-term-green          fg-term-green)
          (fg-term-yellow         "#a2734c")
          (bg-term-yellow         fg-term-yellow)
          (fg-term-blue           "#12488b")
          (bg-term-blue           fg-term-blue)
          (fg-term-magenta        "#a347ba")
          (bg-term-magenta        fg-term-magenta)
          (fg-term-cyan           "#2aa1b3")
          (bg-term-cyan           fg-term-cyan)
          (fg-term-white          "#cfcfcf")
          (bg-term-white          fg-term-white)
          (fg-term-black-bright   "#5d5d5d")
          (bg-term-black-bright   fg-term-black-bright)
          (fg-term-red-bright     "#f66151")
          (bg-term-red-bright     fg-term-red-bright)
          (fg-term-green-bright   "#33d17a")
          (bg-term-green-bright   fg-term-green-bright)
          (fg-term-yellow-bright  "#e9ad0c")
          (bg-term-yellow-bright  fg-term-yellow-bright)
          (fg-term-blue-bright    "#2a7bde")
          (bg-term-blue-bright    fg-term-blue-bright)
          (fg-term-magenta-bright "#c061cb")
          (bg-term-magenta-bright fg-term-magenta-bright)
          (fg-term-cyan-bright    "#33c7de")
          (bg-term-cyan-bright    fg-term-cyan-bright)
          (fg-term-white-bright   "#ffffff")
          (bg-term-white-bright   fg-term-white-bright)))

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
        (date-deadline red-warmer)
        (date-event magenta-warmer)
        (date-now yellow-warmer)
        (date-scheduled magenta-cooler)))

  (setq modus-themes-variable-pitch-ui nil
        modus-themes-italic-constructs nil
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts nil))

(use-package auto-dark
  :ensure t
  :defer t
  :hook ((after-init . auto-dark-mode))
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
        ;; ispell-word at bottom
        ("\\*Choices\\*"
         (display-buffer-below-selected)
         (window-height . fit-window-to-buffer))
        ))

;; Whitespace
(setq whitespace-style '(face indentation tabs tab-mark spaces space-mark
                              newline newline-mark trailing))

(provide 'chbm-style)
