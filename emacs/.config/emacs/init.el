;; Custom functions ============================================================

(defun chbm-set-fonts (&rest _)
  "Set fonts for frame and after theme"
  (when (display-graphic-p)
    (set-face-attribute 'default nil :family "Iosevka" :height 120)
    (set-face-attribute 'fixed-pitch nil :family "Iosevka")
    (set-face-attribute 'variable-pitch nil :family "Iosevka")
    (set-face-attribute 'tab-bar nil :height 120)
    (set-face-attribute 'tab-bar-tab nil :height 120)
    (set-face-attribute 'tab-bar-tab-inactive nil :height 120)))

(defun chbm-start-with-agenda ()
  (when (and (display-graphic-p)
             (not (string-prefix-p " *" (buffer-name))))
    (org-agenda-list)
    (delete-other-windows)))

;; Thanks Prot!
(defun prot-spell-ispell-display-buffer (buffer)
  "Function to override `ispell-display-buffer' for BUFFER.
Use this as `advice-add' to override the aforementioned Ispell
function.  Then you can control the buffer's specifics via
`display-buffer-alist' (how it ought to be!)."
  (pop-to-buffer buffer)
  (set-window-point (get-buffer-window buffer) (point-min)))

(defun chbm-move-window-in-direction (dir)
  (condition-case error
      (windmove-swap-states-in-direction dir)
    (user-error
     (let* ((sel-win (selected-window))
            (root-win (frame-root-window))
            (sel-win-par (window-parent sel-win)))
       (if (eq root-win sel-win-par)
           (user-error (concat "Cannot move window "
                               (symbol-name dir)
                               ", window is alread at the edge"))
         (let* ((width (window-total-width sel-win))
                (height (window-total-height sel-win))
                (new-win (split-window (window-parent sel-win) nil dir)))
           (delete-window sel-win)
           (select-window new-win)))))))

(defun chbm-move-window-right ()
  (interactive)
  (chbm-move-window-in-direction 'right))

(defun chbm-move-window-left ()
  (interactive)
  (chbm-move-window-in-direction 'left))

(defun chbm-move-window-above ()
  (interactive)
  (chbm-move-window-in-direction 'above))

(defun chbm-move-window-below ()
  (interactive)
  (chbm-move-window-in-direction 'below))


;; Main emacs config  ==========================================================

(use-package emacs
  :bind (;; Disabling some bindings
         ("C-z" . nil)                  ; minimize
         ("C-x C-d" . nil)              ; list directory
         ("C-h h" . nil)                ; "hello" buffer
         ("C-x C-c" . nil)              ; Closing emacs 🙂 (actually gets rebound later)
         ("M-`" . nil)                  ; menu bar in the minibuffer
         ;; Rebinds
         ("M-o" . other-window)
         ("C-x C-c C-c" . save-buffers-kill-emacs)
         ("C-x C-r" . restart-emacs)
         ("M-z" . zap-up-to-char)       ; zap-up-to-char instead of zap-to-char
         ("M-c" . capitalize-dwim)
         ("M-l" . downcase-dwim)
         ("M-u" . upcase-dwim)
         ("C-S-d" . duplicate-line)
         ("C-x C-b" . ibuffer))
  :hook (((prog-mode text-mode) . (lambda ()
                                    (setq show-trailing-whitespace t)
                                    (column-number-mode)
                                    (display-line-numbers-mode)))
         ((org-mode text-mode) . auto-fill-mode)
         (server-after-make-frame . (lambda ()
                                      (chbm-set-fonts)
                                      (chbm-start-with-agenda))))
  :ensure nil
  :init
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  :config
  ;; Smooth scroll
  (pixel-scroll-precision-mode 1)
  (pixel-scroll-mode 1)

  ;; Hiding ui elements
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0)

  ;; Remembering previous window configuration
  (winner-mode 1)

  ;; Minibuffer improvements
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1)

  ;; Fix for auto-dark where
  (advice-add 'load-theme :after #'chbm-set-fonts)

  ;; Trying to properly set fonts
  (chbm-set-fonts)

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

  (setq read-extended-command-predicate #'command-completion-default-include-p)

  ;; Indentation
  (setq indent-line-function 'insert-tab)
  (setq tab-always-indent 'complete)

  ;; Tab bar options
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-new-button-show nil)
  (setq tab-bar-show 1)

  (setq deleted-by-moving-to-trash t)

  ;; Enable only git for VC
  (setq vc-handled-backends '(Git))

  ;; Frame options
  (setq frame-resize-pixelwise t)
  (setq default-frame-alist '((font . "Iosevka-12")
                              (width . (text-pixels . 800))
                              (height . (text-pixels . 600))
                              (vertical-scroll-bars . nil)
                              (tab-bar-lines . 0)))

  (advice-add #'ispell-display-buffer :override #'prot-spell-ispell-display-buffer)

  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil)
  (setq switch-to-buffer-in-dedicated-window 'pop)
  (setq split-height-threshold 85)
  (setq split-width-threshold 125)
  (setq window-min-height 3)
  (setq window-min-width 30)

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
  (setq use-short-answers t)
  (setq async-shell-command-buffer 'new-buffer))

;; Theming  ====================================================================

(use-package modus-themes
  :config
  (setq modus-themes-common-palette-overrides
        `((cursor magenta-warmer)
          (bg-region bg-lavender)
          (fg-region unspecified)

	      (string green-cooler)
	      (bg-changed-fringe bg-cyan-intense)

	      ;; Blue modeline
	      (bg-mode-line-active bg-blue-intense)
	      (fg-mode-line-active fg-main)

	      ;; Number line invisible
	      (fringe unspecified)
	      (fg-line-number-inactive comment)
	      (fg-line-number-active cursor)
	      (bg-line-number-inactive unspecified)
	      (bg-line-number-active unspecified)

          ;; Org mode
          (fg-heading-0 blue-cooler)
          (fg-heading-1 magenta-cooler)
          (fg-heading-2 magenta-warmer)
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
          (date-weekend blue-faint)
          ))
  (setq modus-themes-variable-pitch-ui t)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-mixed-fonts t))

;; Modeline  ===================================================================
;; Most stuff here comes from Prot

(defface chbm-modeline-green-bg
  (modus-themes-with-colors
    `((t :inherit bold :foreground ,green :background ,bg-green-intense :box ,green)))
  "Face for modeline indicators with a background."
  :group 'chbm-modeline-faces)

(defface chbm-modeline-magenta-bg
  (modus-themes-with-colors
    `((t :inherit bold :foreground ,magenta :background ,bg-magenta-intense :box ,magenta)))
  "Face for modeline indicators with a background."
  :group 'chbm-modeline-faces)

(defface chbm-modeline-red-bg
  (modus-themes-with-colors
    `((t :inherit bold :foreground ,red :background ,bg-red-intense :box ,red)))
  "Face for modeline indicators with a background."
  :group 'chbm-modeline-faces)

(defface chbm-modeline-red-fg
  (modus-themes-with-colors
    `((t :foreground ,red)))
  "Face for modeline indicators with a background."
  :group 'chbm-modeline-faces)

(defvar-local chbm-modeline-kbd-macro
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
        (propertize " KMacro " 'face 'chbm-modeline-green-bg)))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")

(defun chbm-modeline-buffer-name ()
  "Return buffer name, with read-only indicator if relevant."
  (let ((name (buffer-name)))
    (if buffer-read-only
        (format "%s %s" (char-to-string #xE0A2) name)
      name)))

(defun chbm-modeline-buffer-identification-face ()
  "Return appropriate face or face list for `chbm-modeline-buffer-identification'."
  (let ((file (buffer-file-name)))
    (cond
     ((and (mode-line-window-selected-p)
           file
           (buffer-modified-p))
      '(italic mode-line-buffer-id))
     ((and file (buffer-modified-p))
      'italic)
     ((mode-line-window-selected-p)
      'mode-line-buffer-id))))

(defvar-local chbm-modeline-buffer-identification
    '(:eval
      (propertize (chbm-modeline-buffer-name)
                  'face (chbm-modeline-buffer-identification-face)
                  'mouse-face 'mode-line-highlight
                  'help-echo (chbm-modeline-buffer-name-help-echo)))
  "Mode line construct for identifying the buffer being displayed.
Propertize the current buffer with the `mode-line-buffer-id'
face.  Let other buffers have no face.")

(defun chbm-modeline-buffer-name-help-echo ()
  "Return `help-echo' value for `chbm-modeline-buffer-identification'."
  (concat
   (propertize (buffer-name) 'face 'mode-line-buffer-id)
   "\n"
   (propertize
    (or (buffer-file-name)
        (format "No underlying file.\nDirectory is: %s" default-directory))
    'face 'font-lock-doc-face)))

(defvar-local chbm-modeline-remote-status
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (file-remote-p default-directory))
        (propertize " @ "
                    'face 'chbm-modeline-magenta-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct for showing remote file name.")

(defvar-local chbm-modeline-window-dedicated-status
    '(:eval
      (when
          (and (mode-line-window-selected-p)
               (window-dedicated-p))
        (propertize " = "
                    'face 'chbm-modeline-red-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct for dedicated window indicator.")

(defun chbm-modeline-major-mode-indicator ()
  "Return appropriate propertized mode line indicator for the major mode."
  (let ((indicator (cond
                    ((derived-mode-p 'text-mode) "§")
                    ((derived-mode-p 'prog-mode) "λ")
                    ((derived-mode-p 'comint-mode) ">_")
                    (t "◦"))))
    (propertize indicator 'face 'shadow)))

(defun chbm-modeline-major-mode-name ()
  "Return capitalized `major-mode' without the -mode suffix."
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))

(defun chbm-modeline-major-mode-help-echo ()
  "Return `help-echo' value for `chbm-modeline-major-mode'."
  (if-let* ((parent (get major-mode 'derived-mode-parent)))
      (format "Symbol: `%s'.  Derived from: `%s'" major-mode parent)
    (format "Symbol: `%s'." major-mode)))

(defvar-local chbm-modeline-major-mode
    (list
     (propertize "%[" 'face 'chbm-modeline-red-fg)
     '(:eval
       (propertize (concat
                    (chbm-modeline-major-mode-indicator)
                    " "
                    (chbm-modeline-major-mode-name))
                   'mouse-face 'mode-line-highlight
                   'help-echo (chbm-modeline-major-mode-help-echo)))
     (propertize "%]" 'face 'chbm-modeline-red-fg))
  "Mode line construct for displaying major modes.")

(declare-function vc-git--symbolic-ref "vc-git" (file))

(defun chbm-modeline--vc-branch-name (file backend)
  "Return capitalized VC branch name for FILE with BACKEND."
  (when-let* ((rev (vc-working-revision file backend))
              (branch (or (vc-git--symbolic-ref file)
                          (substring rev 0 7))))
    (capitalize branch)))

(declare-function vc-git-working-revision "vc-git" (file))

(defvar chbm-modeline-vc-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'vc-diff)
    (define-key map [mode-line down-mouse-3] 'vc-root-diff)
    map)
  "Keymap to display on VC indicator.")

(defun chbm-modeline--vc-help-echo (file)
  "Return `help-echo' message for FILE tracked by VC."
  (format "Revision: %s\nmouse-1: `vc-diff'\nmouse-3: `vc-root-diff'"
          (vc-working-revision file)))

(defun chbm-modeline--vc-text (file branch &optional face)
  "Prepare text for Git controlled FILE, given BRANCH.
With optional FACE, use it to propertize the BRANCH."
  (concat
   (propertize (char-to-string #xE0A0) 'face 'shadow)
   " "
   (propertize branch
               'face face
               'mouse-face 'mode-line-highlight
               'help-echo (chbm-modeline--vc-help-echo file)
               'local-map chbm-modeline-vc-map)))

(defun chbm-modeline--vc-details (file branch &optional face)
  "Return Git BRANCH details for FILE, truncating it if necessary.
The string is truncated if the width of the window is smaller
than `split-width-threshold'."
  (chbm-modeline--vc-text file branch face))

(defvar chbm-modeline--vc-faces
  '((added . vc-locally-added-state)
    (edited . vc-edited-state)
    (removed . vc-removed-state)
    (missing . vc-missing-state)
    (conflict . vc-conflict-state)
    (locked . vc-locked-state)
    (up-to-date . vc-up-to-date-state))
  "VC state faces.")

(defun chbm-modeline--vc-get-face (key)
  "Get face from KEY in `chbm-modeline--vc-faces'."
  (alist-get key chbm-modeline--vc-faces 'vc-up-to-date-state))

(defun chbm-modeline--vc-face (file backend)
  "Return VC state face for FILE with BACKEND."
  (when-let* ((key (vc-state file backend)))
    (chbm-modeline--vc-get-face key)))

(defvar-local chbm-modeline-vc-branch
    '(:eval
      (when-let* (((mode-line-window-selected-p))
                  (file (or buffer-file-name default-directory))
                  (backend (or (vc-backend file) 'Git))
                  (branch (chbm-modeline--vc-branch-name file backend))
                  (face (chbm-modeline--vc-face file backend)))
        (chbm-modeline--vc-details file branch face)))
  "Mode line construct to return propertized VC branch.")

(defvar-local chbm-modeline-diagnostics
    '(:eval
      (when-let* (((mode-line-window-selected-p))
                  (eglot '(eglot--managed-mode ("[" eglot--mode-line-format "]")))
                  (flymake (if (bound-and-true-p flymake-mode)
                               '(flymake-mode-line-exception flymake-mode-line-counters)
                             nil)))
        (list eglot flymake))
      "Mode line construct to return propertized buffer diagnostics."))


(dolist (construct '(chbm-modeline-kbd-macro
                     chbm-modeline-remote-status
                     chbm-modeline-buffer-identification
                     chbm-modeline-window-dedicated-status
                     chbm-modeline-major-mode
                     chbm-modeline-vc-branch
                     chbm-modeline-diagnostics
                     ))
  (put construct 'risky-local-variable t))

(setq-default mode-line-format
              '("%e"
                chbm-modeline-kbd-macro
                chbm-modeline-remote-status
                chbm-modeline-window-dedicated-status
                "  "
                chbm-modeline-buffer-identification
                "  "
                chbm-modeline-major-mode
                mode-line-process

                mode-line-format-right-align

                chbm-modeline-diagnostics
                "  "
                chbm-modeline-vc-branch
                " %p %l:%c  "))

;; Frames and window utilities =================================================

(use-package transpose-frame)

(use-package windmove
  :ensure nil
  :hook (after-init . windmove-mode)
  :bind (("C-S-<right>" . chbm-move-window-right)
         ("C-S-<left>" . chbm-move-window-left)
         ("C-S-<up>" . chbm-move-window-above)
         ("C-S-<>" . chbm-move-window-below))
  :config
  (windmove-default-keybindings 'ctrl)
  ;; (windmove-swap-states-default-keybindings '(ctrl shift))
  (windmove-delete-default-keybindings)
  )

(use-package auto-dark
  :hook ((auto-dark-dark-mode . chbm-set-fonts)
         (auto-dark-light-mode . chbm-set-fonts)
         (after-init . (lambda ()
                         (setq auto-dark-themes '((modus-vivendi) (modus-operandi)))
                         (auto-dark-mode)))))

;; Misc essential packages =====================================================

(use-package which-key
  :ensure nil
  :hook (after-init . which-key-mode))

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

(use-package no-littering
  :demand t
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
  (no-littering-theme-backups)
  (let ((dir (no-littering-expand-var-file-name "lock-files/")))
    (make-directory dir t)
    (setq lock-file-name-transforms `((".*" ,dir t)))))

(use-package direnv
  :hook (after-init . direnv-mode))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode))

(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-fringe-mark nil)
  (setq bookmark-save-flag 1))

(use-package project
  :ensure nil
  :config
  (add-to-list 'project-switch-commands '(magit-project-status "Magit" "m")))

;; Minibuffer ==================================================================

(use-package vertico
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-count 10))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-additional-variables '(register-alist kill-ring)))

;; Completion frameworks =======================================================

(use-package corfu
  :bind (:map corfu-map
              ("RET" . nil))
  :hook ((inferior-python-mode . (lambda () (corfu-auto nil)))
         (after-init . global-corfu-mode)
         (after-init . corfu-popupinfo-mode))
  :config
  ;; Autocomplete settings
  (setq corfu-auto t)
  (setq corfu-auto-prefix 2)
  (setq corfu-auto-delay 0.2)
  (setq corfu-quit-no-match 'separator)
  (setq corfu-quit-at-boundary t)
  (setq corfu-preview-current nil)

  (setq corfu-cycle t)
  (setq corfu-popupinfo-delay 0.5)
  (setq corfu-left-margin-width 0)
  (setq corfu-right-margin-width 0))

(use-package orderless
  :config
  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles basic partial-completion))))
  (setq completion-matching-styles '(orderless-regexp)))

;; Searching ===================================================================

(use-package isearch
  :ensure nil
  :config
  (setq isearch-lazy-count t)
  (setq isearch-lazy-count-prefix-format "%s/%s")
  (setq isearch-lazy-count-suffix-format nil)
  (setq isearch-lax-whitespace t)
  (setq search-whitespace-regexp ".*?"))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-preview-key "M-.")
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<"))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)
   :map minibuffer-local-map
   ("C-c C-c" . embark-collect)
   ("C-c C-e" . embark-export)))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Useful buffer types =========================================================

(use-package comint                     ; Repls
  :ensure nil
  :hook
  (comint-output-filter-functions . comint-osc-process-output)
  :config
  (setq ansi-color-for-comint-mode t)
  (setq comint-prompt-read-only t)
  (setq comint-completion-autolist t)
  (setq comint-input-ignoredups t)
  (setq-default comint-scroll-to-bottom-on-input t)
  (setq-default comint-scroll-to-bottom-on-output nil)
  (setq-default comint-input-autoexpand 'input))

(use-package compile
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter)
  :config
  (setq ansi-color-for-compilation-mode t)
  (setq compilation-skip-threshold 2))

(use-package ediff
  :ensure nil
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor t)
  (setq ediff-show-clashes-only t))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :hook (dired-mode . (lambda () (dired-hide-details-mode 1)))
  :config
  (setq dired-listing-switches "-aBhlv  --group-directories-first")
  (setq dired-dwim-target t)
  (setq wdired-allow-to-change-permissions t))

(use-package magit
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package wgrep
  :bind (:map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))

(use-package tramp
  :ensure nil
  :config
  (setq remote-file-name-inhibit-locks t
        tramp-use-scp-direct-remote-copying t
        remote-file-name-inhibit-auto-save-visited t
        tramp-copy-size-limit (* 1024 1024) ;; 1MB
        tramp-verbose 2)

  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)

  (setq magit-tramp-pipe-stty-settings 'pty)
  (with-eval-after-load 'tramp
    (with-eval-after-load 'compile
      (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options))))

;; Editor enhancements =========================================================

(use-package electric
  :ensure nil
  :hook ((after-init . electric-pair-mode)
         (after-init . electric-indent-mode))
  :config
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package paren
  :ensure nil
  :config
  (setq show-paren-when-point-in-periphery t)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-context-when-offscreen 'overlay))

(use-package diff-hl
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (after-init . global-diff-hl-mode)
         (after-init . diff-hl-flydiff-mode)))

(use-package flymake
  :ensure nil
  :config
  (setq flymake-indicator-type 'margins))

(use-package surround
  :ensure t
  :bind-keymap ("C-c s" . surround-keymap))

;; Languages related modes =====================================================

(use-package treesit-auto
  :hook (after-init . (lambda () (global-treesit-auto-mode 1)))
  :config
  (setq treesit-auto-install 'prompt)
  (setq treesit-auto-add-to-auto-mode-alist 'all)
  (setq-default treesit-font-lock-level 4))


(use-package eglot
  :ensure nil
  :hook ((c++-ts-mode python-ts-mode cmake-ts-mode racket-mode haskell-mode)
         . eglot-ensure)
  :bind (("C-x C-a" . eglot-code-actions)
         ("C-x C-r" . eglot-rename))
  :config
  (setq eglot-autoshutdown t)
  (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider :documentOnTypeFormattingProvider)
  (add-to-list 'eglot-server-programs
               '((org-mode (LaTeX-mode :language-id "latex")) . ("ltex-ls-plus" "--server-type" "TcpSocket" "--port" :autoport)))
  (setq-default eglot-workspace-configuration
                '((:ltex . (:language "auto"
                            :completionEnabled t
                            :latex (:environments (:lstlisting "ignore" :circuitikz "ignore" )
                                    :commands (:\\lstset{} "ignore"
                                               :\\lstdefinelanguage{}{} "ignore"
                                               :\\lstinputlisting{} "ignore"))
                            :disabledRules (:fr ["FRENCH_WHITESPACE"]))))))

(use-package eglot-inactive-regions
  :hook (after-init . (lambda () (eglot-inactive-regions-mode 1)))
  :config
  (setq eglot-inactive-regions-style 'darken-foreground)
  (setq eglot-inactive-regions-opacity 0.4))

(use-package racket-mode
  :mode "\\.rkt\\'")

(use-package auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default Tex-master nil))

(use-package haskell-mode
  :mode "\\.hs\\'"
  :hook (haskell-mode . interactive-haskell-mode))

(defun chbm-indent-style()
    "Override the built-in BSD indentation style with some additional rules"
    `(
      ((node-is ")") parent-bol 0)
      ((node-is "(") parent-bol 0)
      ((node-is "}") parent-bol 0)
      ((node-is "{") parent-bol 0)
      ((n-p-gp nil nil "namespace_definition") grand-parent 0)
      ((node-is "preproc") column-0 0)
      ((node-is "access_specifier") parent-bol 2)
      ((node-is "field_initializer_list") parent-bol 4)
      ((node-is "field_initializer") (nth-sibling 1) 0)
      ((parent-is "compound_statement") parent-bol c-ts-mode-indent-offset)
      ((node-is "compound_statement") parent-bol 0)
      ((parent-is "parameter_list") parent-bol 4)
      ((parent-is "argument_list") parent-bol 4)
      ((parent-is "initializer_list") grand-parent c-ts-mode-indent-offset)

      ,@(alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))

(use-package c-ts-mode
  :ensure nil
  :bind ("C-c o" . ff-find-other-file)
  :config
  (setq-default c-ts-mode-indent-offset 4)
  (setq-default c-ts-mode-indent-style #'chbm-indent-style))

(use-package xml-mode
  :ensure nil
  :mode "\\.cts")

;; Org =========================================================================

(use-package org
  :ensure nil
  :demand t
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  (require 'org-tempo)
  (setq org-directory "~/Notes"
        org-icalendar-use-scheduled '(event-if-not-todo event-if-todo event-if-todo-not-done todo-start)
        org-icalendar-use-deadline '(event-if-not-todo event-if-todo event-if-todo-not-done todo-due)
        org-agendas-directory (concat org-directory "/Agendas/")
        org-default-notes-file (expand-file-name (concat org-agendas-directory "Misc.org") org-directory)
        org-agenda-files (directory-files-recursively org-agendas-directory  "\\.org$")
        org-archive-location (concat org-directory "/archive.org::datetree/")
        org-attach-id-dir (concat org-directory "/Data/")
        org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w)" "DONE(d)"))))

(use-package org-roam
  :bind (("C-c C-r c" . org-roam-capture)
         ("C-c C-r f" . org-roam-node-find))
  :config
  (setq org-roam-directory (concat org-directory "/Roam/"))
  (org-roam-db-autosync-mode 1))
