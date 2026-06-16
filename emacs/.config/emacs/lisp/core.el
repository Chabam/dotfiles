;;; Useful functions

(defun chbm/copy-pwd (&optional abrev)
  (interactive "P")
  (kill-new (if abrev
                (abbreviate-file-name default-directory)
              default-directory)))

(defun chbm/ansi-colorize-buffer ()
  (interactive)
  (mark-whole-buffer)
  (ansi-color-apply-on-region (point-min)
                              (point-max)))

(use-package emacs
  :bind (;; Disabling some bindings
         ("C-z" . nil)                  ; minimize
         ("C-h h" . nil)                ; "hello" buffer
         ("C-x C-c" . nil)              ; Closing emacs 🙂 (actually gets rebound later)
         ("M-`" . nil)                  ; menu bar in the minibuffer
         ("C-x C-z" . nil)              ; Minimize
         ;; rebinds
         ("C-x M-g" . grep)
         ("C-x M-e" . eshell)
         ("M-o" . other-window)
         ("C-M-o" . other-frame)
         ("C-x C-c C-c" . save-buffers-kill-emacs)
         ("M-z" . zap-up-to-char)       ; zap-up-to-char instead of zap-to-char
         ("C-M-z" . delete-pair)
         ("M-c" . capitalize-dwim)
         ("M-l" . downcase-dwim)
         ("M-u" . upcase-dwim)
         ("C-c t d" . delete-trailing-whitespace)
         ("C-c k p" . chbm/copy-pwd)
         ("C-x r y" . chbm/yank-copied-rectangle-as-lines)
         ("C-S-d" . duplicate-dwim)
         ("C-x C-b" . ibuffer))
  :hook (((prog-mode text-mode) . (lambda ()
                                    (setq show-trailing-whitespace t)
                                    (column-number-mode)
                                    (display-line-numbers-mode)))
         ((org-mode text-mode) . auto-fill-mode)
         (server-after-make-frame . (lambda ()
                                      (chbm/set-fonts))))
  :ensure nil
  :init
  (setq load-prefer-newer t)

  (when (file-exists-p "/usr/share/emacs/site-lisp")
    (add-to-list 'load-path "/usr/share/emacs/site-lisp" t))

  (require 'package)
  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)

  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))

  (if (file-exists-p custom-file)
      (load-file custom-file)
    (write-region "" nil custom-file))

  :config
  ;; Smooth scroll
  (pixel-scroll-precision-mode 1)
  (pixel-scroll-mode 1)

  ;; Prettify diffs by putting +/- in the fringe
  (setq diff-font-lock-prettify t)

  ;; Enables faster scrolling. This may result in brief periods of inaccurate
  ;; syntax highlighting, which should quickly self-correct.
  (setq fast-but-imprecise-scrolling t)

  (setq-default display-line-numbers-width 3)
  (setq-default display-line-numbers-widen t)


  ;; Hiding ui elements
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0)

  (setq undo-limit (* 13 160000))
  (setq undo-strong-limit (* 13 240000))
  (setq undo-outer-limit (* 13 24000000))

  ;; Disable ellipsis when printing s-expressions in the message buffer
  (setq eval-expression-print-length nil)
  (setq eval-expression-print-level nil)

  ;; This directs gpg-agent to use the minibuffer for passphrase entry
  (setq epg-pinentry-mode 'loopback)

  ;; Minibuffer improvements
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1)

  ;; Keep the cursor out of the read-only portions of the.minibuffer
  (setq minibuffer-prompt-properties
        '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Trying to properly set fonts
  (chbm/set-fonts)

  ;; Disable fontification during user input to reduce lag in large buffers.
  ;; Also helps marginally with scrolling performance.
  (setq redisplay-skip-fontification-on-input t)

  ;; Deleting selection when typing
  (delete-selection-mode 1)

  ;; Killing Emacs? Why would you?
  (setq confirm-kill-emacs 'y-or-n-p)

  ;; Remove blinking for delete-pair
  (setq delete-pair-blink-delay 0)

  ;; Default indentation
  (setq-default standard-indent 4
                tab-width 4
                indent-tabs-mode nil)

  (setq custom-safe-themes t)
  (setq use-package-always-ensure t)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Hide commands in `M-x' which do not work in the current mode.
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Remove the bell
  (setq ring-bell-function 'ignore)

  ;; Remove splash screen
  (setq inhibit-startup-screen t)

  ;; Completion stuff
  (setq text-mode-ispell-word-completion nil)
  (setq completion-styles '(basic substring partial-completion flex))
  (setq tab-always-indent 'complete)
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  ;; Tab bar options
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-new-button-show nil)
  (setq tab-bar-show 1)

  (setq delete-by-moving-to-trash t)

  ;; Enable only git for VC
  (setq vc-handled-backends '(Git))

  ;; Frame options
  (setq frame-resize-pixelwise t)
  (setq frame-inhibit-implied-resize t)

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
  ;; y or n instead of yes or no
  (setq use-short-answers t)

  ;; Don't ask for creating new buffers on async commands
  (setq async-shell-command-buffer 'new-buffer)

  (setq package-install-upgrade-built-in t)

  (setq vc-follow-symlinks t)

  ;; Fix for dape
  (setq window-sides-vertical t)

  ;; Enabling some disabled commands
  (put 'narrow-to-region 'disabled nil))

;;; Temp files cleanup

(use-package no-littering
  :demand t
  :config
  (no-littering-theme-backups)
  (let ((dir (no-littering-expand-var-file-name "lock-files/")))
    (make-directory dir t)
    (setq lock-file-name-transforms `((".*" ,dir t)))))

;;; File managing

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :config
  (setq dired-listing-switches "-aBhlv  --group-directories-first")
  (setq dired-dwim-target t)
  (setq dired-vc-rename-file t)
  (setq dired-vc-rename-file t)
  (setq wdired-allow-to-change-permissions t)
  (setq dired-movement-style 'bounded-files)

  (when chbm/emacs-containerized
    (define-key dired-mode-map (kbd "E") #'chbm/dired-do-open-containerized)))

;;; Remote access

(use-package tramp
  :ensure nil
  :config
  (setq remote-file-name-inhibit-locks t)
  (setq tramp-use-scp-direct-remote-copying t)
  (setq remote-file-name-inhibit-auto-save-visited t)
  (setq tramp-copy-size-limit (* 1024 1024)) ;; 1mb
  (setq remote-file-name-inhibit-cache 50)
  (setq remote-file-name-inhibit-locks t)
  (setq remote-file-name-inhibit-delete-by-moving-to-trash t)
  (setq tramp-verbose 1))

;;; Keybinds

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

;;; Projects

(use-package direnv
  :hook (after-init . direnv-mode))

(use-package project
  :ensure nil
  :config
  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-find-regexp "Find regexp")
          (project-find-dir "Find directory")
          (project-dired "Project Dired" "D")
          (project-vc-dir "VC-Dir")
          (magit-project-status "Magit" "m")
          (project-eshell "Eshell")
          (project-any-command "Other")))
  (setq project-mode-line t)
  (setq project-compilation-buffer-name-function 'project-prefixed-buffer-name))

;;; Buffers and files quick access

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-max-saved-items 300)
  (setq recentf-max-menu-items 15))

(use-package saveplace
  :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook (after-init . save-place-mode)
  :init
  (setq save-place-limit 400))

(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-fringe-mark nil)
  (setq bookmark-save-flag 1))

(use-package wgrep)

(use-package isearch
  :ensure nil
  :config
  (setq isearch-lazy-count t)
  (setq isearch-lazy-count-prefix-format "%s/%s")
  (setq isearch-lazy-count-suffix-format nil))

(use-package man
  :ensure nil
  :config
  (setq Man-notify-method 'thrifty))

;; Thanks Prot!
(defun prot-spell-ispell-display-buffer (buffer)
  "Function to override `ispell-display-buffer' for BUFFER.
Use this as `advice-add' to override the aforementioned Ispell
function.  Then you can control the buffer's specifics via
`display-buffer-alist' (how it ought to be!)."
  (pop-to-buffer buffer)
  (set-window-point (get-buffer-window buffer) (point-min)))

(use-package ispell
  :ensure nil
  :config
  (setq ispell-program-name "hunspell"))

(use-package winner
  :hook (after-init . (lambda () (winner-mode 1))))
