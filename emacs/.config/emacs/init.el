;; -*- lexical-binding: t -*-
;;; Custom functions

(defun chbm-set-fonts (&optional font-size &rest _)
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

(defun ansi-colorize-buffer ()
  (interactive)
  (mark-whole-buffer)
  (ansi-color-apply-on-region (point-min)
                              (point-max)))

(defun emacs-wiki-register-signals (client-path)
  "Register for the 'QueryEndSession' and 'EndSession' signals from
Gnome SessionManager.

When we receive 'QueryEndSession', we just respond with
'EndSessionResponse(true, \"\")'.  When we receive 'EndSession', we
append this EndSessionResponse to kill-emacs-hook, and then call
kill-emacs.  This way, we can shut down the Emacs daemon cleanly
before we send our 'ok' to the SessionManager."
  (setq my-gnome-client-path client-path)
  (let ((end-session-response (lambda (&optional arg)
                                 (dbus-call-method-asynchronously
                                  :session "org.gnome.SessionManager" my-gnome-client-path
                                  "org.gnome.SessionManager.ClientPrivate" "EndSessionResponse" nil
                                  t ""))))
    (dbus-register-signal
     :session "org.gnome.SessionManager" my-gnome-client-path
     "org.gnome.SessionManager.ClientPrivate" "QueryEndSession"
     end-session-response)
    (dbus-register-signal
     :session "org.gnome.SessionManager" my-gnome-client-path
     "org.gnome.SessionManager.ClientPrivate" "EndSession"
     `(lambda (arg)
        (add-hook 'kill-emacs-hook ,end-session-response t)
        (kill-emacs)))))

(defun chbm-ff-find-other-file ()
  (interactive)
  (ff-find-other-file nil t))

(defun chbm-is-first-sibling (&optional node-t parent-node-t)
  (lambda (node parent &rest _)
    (and node
         parent
         (or (not node-t)
             (string-match-p
              node-t (treesit-node-type node)))
         (or (not parent-node-t)
             (string-match-p
              parent-node-t (treesit-node-type parent)))
         (not (treesit-node-prev-sibling node t)))))

(defun chbm-is-last-sibling (&optional node-t parent-node-t)
  (lambda (node parent &rest _)
    (and node
         parent
         (or (not node-t)
             (string-match-p
              node-t (treesit-node-type node)))
         (or (not parent-node-t)
             (string-match-p
              parent-node-t (treesit-node-type parent)))
         (not (treesit-node-next-sibling node t)))))

(defun chbm-bsd-style-indent ()
  "Override the built-in BSD indentation style with some additional rules"
  `(
    ((n-p-gp nil "declaration_list" "namespace_definition") parent-bol 0)
    ((match "compound_statement" "for_range_loop") standalone-parent 0)
    ((node-is "access_specifier") parent-bol 2)
    ((match "field_declaration_list" "struct_specifier") parent-bol 0)
    ((match "compound_statement" "lambda_expression") parent-bol c-ts-mode-indent-offset)
    ((node-is "field_initializer_list") parent-bol c-ts-mode-indent-offset)
    ((parent-is "field_initializer_list") grand-parent c-ts-mode-indent-offset)
    ((chbm-is-first-sibling "parameter_declaration" "parameter_list") standalone-parent c-ts-mode-indent-offset)
    ((chbm-is-first-sibling nil "argument_list") standalone-parent c-ts-mode-indent-offset)
    ((chbm-is-last-sibling ")" "parameter_list") standalone-parent 0)
    ((chbm-is-last-sibling ")" "argument_list") standalone-parent 0)
    ((parent-is "argument_list") prev-sibling 0)
    ((parent-is "parameter_list") prev-sibling 0)
    ,@(alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))

(defun chbm-set-c-style-indent ()
  "Sets up clang-format if the proper file is there otherwise use the default treesit"
  (when (locate-dominating-file default-directory ".clang-format")
    (setq-local indent-region-function #'clang-format-region))
  (setq-local c-ts-mode-indent-offset 4)
  (c-ts-mode-set-style 'chbm-bsd-style-indent))

(defun chbm-set-cc-search-dirs-project ()
  "Adds likely locations to look for other files based on the project"
  (interactive)
  (when (project-current)
    (let* ((project-root (caddr (project-current)))
           (is-not-hidden-p (lambda (f) (null (string-match "^\\." f))))
           (is-not-build-dir-p (lambda (f) (null (string-match "build" f))))
           (dir-recurse-p (lambda (p)
                            (let ((f (file-name-nondirectory p)))
                              (and (funcall is-not-build-dir-p f)
                                   (funcall is-not-hidden-p f)))))
           (exclude-p (lambda (f)
                        (and (file-directory-p f)
                             (funcall is-not-hidden-p f))))
           (get-subdirs (lambda (dir)
                          (when (file-directory-p dir)
                            (seq-filter exclude-p
                                        (directory-files-recursively dir "" t dir-recurse-p)))))
           (sub-dirs (funcall get-subdirs project-root)))
      (setq-local cc-search-directories
                  (append
                   (list
                    "."
                    "/usr/include"
                    "/usr/local/include/*")
                   (mapcar (lambda (dir) (file-name-concat dir "*")) sub-dirs))))))

(defun chbm-yank-copied-rectangle-as-lines ()
  "Insert the last copied or killed rectangle as regular lines."
  (interactive)
  (let ((rec (get-text-property 0 'yank-handler (car kill-ring))))
    (when (not rec)
      (user-error "No rectangle to yank"))
    (insert (substring-no-properties (car kill-ring)))))

(defconst chabam-ca-headers
  (apply 'concat
         '(;; Styling with simplecss
           "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />"
           ;; Syntax hightlighting with highlight.js
           "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/styles/atom-one-dark.min.css\">"
           "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/highlight.min.js\"></script>"
           "<script>document.addEventListener('DOMContentLoaded', () => {
                          document.querySelectorAll('pre.src').forEach((block) => {
                            hljs.highlightElement(block);
                          });
                        });</script>"
           ;; Overriding some styles
           "<style>.tag, .timestamp {
                       font-size: 0.5em;
                       font-size: 1rem;
                       font-family: var(--mono-font);
                       padding: 0.5em;
                       border-radius: var(--standard-border-radius);
                       box-shadow: none;
                       max-width: 100%;
                       background-color: var(--accent-bg);
                     }
            </style>"))
  "The headers used for my website")

(defun chbm-change-org-publish-location (dest)
  (interactive (list (setq dest (read-directory-name "New location: "))))
  (mapc (lambda (conf)
          (plist-put (cdr conf) ':publishing-directory (concat dest (car conf))))
        org-publish-project-alist))

(defun chbm-set-website-config ()
  (let ((conf (locate-dominating-file default-directory "site-config.el")))
    (when conf
      (load-file (concat conf "site-config.el"))
      (message "org-publish site config loaded!"))))

;;; Main emacs config

(use-package emacs
  :bind (;; Disabling some bindings
         ("C-z" . nil)                  ; minimize
         ("C-x C-d" . nil)              ; list directory
         ("C-h h" . nil)                ; "hello" buffer
         ("C-x C-c" . nil)              ; Closing emacs 🙂 (actually gets rebound later)
         ("M-`" . nil)                  ; menu bar in the minibuffer
         ("C-x C-z" . nil)              ; Minimize
         ;; rebinds
         ("M-o" . other-window)
         ("C-x C-c C-c" . save-buffers-kill-emacs)
         ("M-z" . zap-up-to-char)       ; zap-up-to-char instead of zap-to-char
         ("C-M-z" . delete-pair)
         ("M-c" . capitalize-dwim)
         ("M-l" . downcase-dwim)
         ("M-u" . upcase-dwim)
         ("C-x r y" . chbm-yank-copied-rectangle-as-lines)
         ("C-S-d" . duplicate-line)
         ("C-x C-b" . ibuffer))
  :hook (((prog-mode text-mode) . (lambda ()
                                    (setq show-trailing-whitespace t)
                                    (column-number-mode)
                                    (display-line-numbers-mode)))
         ((org-mode text-mode) . auto-fill-mode)
         (server-after-make-frame . (lambda ()
                                      (chbm-set-fonts)
                                      (chbm-start-with-agenda)
                                      )))
  :ensure nil
  :init
  (when (file-exists-p "/usr/share/emacs/site-lisp")
    (add-to-list 'load-path "/usr/share/emacs/site-lisp" t))

  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (setq package-archive-priorities
        '(("gnu" . 3)
          ("melpa" . 2)
          ("nongnu" . 1)))
  (package-initialize)

  (require 'server)
  (when (not (server-running-p)) (server-start))
  ;;; save & shutdown when we get an "end of session" signal on dbus
  (require 'dbus)

  ;; DESKTOP_AUTOSTART_ID is set by the Gnome desktop manager when emacs
  ;; is autostarted.  We can use it to register as a client with gnome
  ;; SessionManager.
  (dbus-call-method-asynchronously
   :session "org.gnome.SessionManager"
   "/org/gnome/SessionManager"
   "org.gnome.SessionManager" "RegisterClient" 'emacs-wiki-register-signals
   "Emacs server" (getenv "DESKTOP_AUTOSTART_ID"))

  :config
  ;; Smooth scroll
  ;; (pixel-scroll-precision-mode 1)
  ;; (pixel-scroll-mode 1)

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

  ;; Deleting selection when typing
  (delete-selection-mode 1)

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
  (setq async-shell-command-buffer 'new-buffer))

;;; Theming

(use-package modus-themes
  :init
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
	      (fg-line-number-inactive fg-dim)
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
  (setq modus-themes-mixed-fonts t)
  (load-theme 'modus-vivendi :noconfirm)
  (load-theme 'modus-operandi :noconfirm))

;;; Modeline
;; Most stuff here comes from Prot

(defface chbm-modeline-green-bg
  '((t ()))
  "Face for modeline indicators with a background."
  :group 'chbm-modeline-faces)

(defun chbm-update-modeline-colors ()
  (modus-themes-with-colors
    (face-spec-set 'chbm-modeline-green-bg
                   `((t :inherit bold :foreground ,green :background ,bg-green-intense :box ,green)))
    (face-spec-set 'chbm-modeline-magenta-bg
                   `((t :inherit bold :foreground ,magenta :background ,bg-magenta-intense :box ,magenta)))
    (face-spec-set 'chbm-modeline-red-bg
                   `((t :inherit bold :foreground ,red :background ,bg-red-intense :box ,red)))
    (face-spec-set 'chbm-modeline-cyan-bg
                   `((t :inherit bold :foreground ,cyan :background ,bg-cyan-intense :box ,cyan)))
    (face-spec-set 'chbm-modeline-red-fg
                   `((t :foreground ,red)))))


(defface chbm-modeline-magenta-bg
  '((t ()))
  "Face for modeline indicators with a background."
  :group 'chbm-modeline-faces)

(defface chbm-modeline-red-bg
  '((t ()))
  "Face for modeline indicators with a background."
  :group 'chbm-modeline-faces)

(defface chbm-modeline-red-fg
  '((t ()))
  "Face for modeline indicators with a background."
  :group 'chbm-modeline-faces)

(defface chbm-modeline-cyan-bg
  '((t ()))
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

(defvar-local chbm-modeline-narrow
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (buffer-narrowed-p)
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        (propertize " Narrow " 'face 'chbm-modeline-cyan-bg)))
  "Mode line construct to report the narrowed state of the current buffer.")

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
    branch))

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
  (when-let ((key (vc-state file backend)))
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
                     chbm-modeline-narrow
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
                chbm-modeline-narrow
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
                " %p %l:%c  ")
              )

;;; Frames and window utilities

(use-package transpose-frame
  :bind (("C-x 5 t" . transpose-frame)))

(use-package windmove
  :ensure nil
  :hook (after-init . windmove-mode)
  :bind (("C-S-<right>" . chbm-move-window-right)
         ("C-S-<left>" . chbm-move-window-left)
         ("C-S-<up>" . chbm-move-window-above)
         ("C-S-<down>" . chbm-move-window-below))
  :config
  (windmove-default-keybindings 'ctrl)
  ;; (windmove-swap-states-default-keybindings '(ctrl shift))
  (windmove-delete-default-keybindings))

(use-package auto-dark
  :hook ((auto-dark-dark-mode . chbm-set-fonts)
         (auto-dark-light-mode . chbm-set-fonts)
         (auto-dark-dark-mode . chbm-update-modeline-colors)
         (auto-dark-light-mode . chbm-update-modeline-colors)
         (after-init . (lambda ()
                         (setq auto-dark-themes '((modus-vivendi) (modus-operandi)))
                         (auto-dark-mode)))))

;;; Misc essential packages

(use-package which-key
  :ensure nil
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-max-display-columns 3)
  (setq which-key-add-column-padding 1)
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
  (add-to-list 'project-switch-commands '(magit-project-status "Magit" "m"))
  (add-to-list 'project-switch-commands '(project-dired "Project Dired" "D")))

(use-package ispell
  :ensure nil
  :config
  (setq ispell-program-name "hunspell"))

;;; Minibuffer

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

;;; Completion frameworks

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

(use-package cape)

;;; Searching

(use-package find-file
  :ensure nil
  :bind (("C-c o" . chbm-ff-find-other-file)))

(use-package isearch
  :ensure nil
  :config
  (setq isearch-lazy-count t)
  (setq isearch-lazy-count-prefix-format "%s/%s")
  (setq isearch-lazy-count-suffix-format nil))

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
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
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

;;; Useful buffer types

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

(use-package eshell
  :ensure nil
  :hook ((eshell-hist-mode . (lambda ()
                               (define-key eshell-hist-mode-map (kbd "C-<up>") nil)
                               (define-key eshell-hist-mode-map (kbd "C-<down>") nil)))))

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

(use-package compile
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter)
  :config
  (setq compilation-scroll-output t)
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
  (setq dired-vc-rename-file t)
  (setq wdired-allow-to-change-permissions t))

(use-package magit
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package wgrep)

(use-package tramp
  :ensure nil
  :config
  (setq remote-file-name-inhibit-locks t
        tramp-use-scp-direct-remote-copying t
        remote-file-name-inhibit-auto-save-visited t
        tramp-copy-size-limit (* 1024 1024) ;; 1mb
        tramp-verbose 2
        enable-remote-dir-locals t))

(use-package pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (pdf-loader-install))

;;; Editor enhancements

(use-package electric
  :ensure nil
  :hook ((after-init . electric-pair-mode)))

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

;;; Languages related modes

(use-package treesit-auto
  :hook (after-init . (lambda () (global-treesit-auto-mode 1)))
  :config
  (setq treesit-auto-install 'prompt)
  (setq treesit-auto-add-to-auto-mode-alist 'all)
  (setq-default treesit-font-lock-level 4))

(use-package eglot
  :ensure nil
  :bind (("C-c e a" . eglot-code-actions)
         ("C-c e r" . eglot-rename)
         ("C-c e s" . eglot))
  :config
  (setq eglot-autoshutdown t)
  (setq eglot-ignored-server-capabilities
      (append eglot-ignored-server-capabilities
              '(:inlayHintProvider :documentOnTypeFormattingProvider :documentOnTypeFormatting)))
  (add-to-list 'eglot-server-programs
               '((org-mode (LaTeX-mode :language-id "latex") text-mode) . ("ltex-ls-plus" "--server-type" "TcpSocket" "--port" :autoport)))
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
  :hook (LaTeX-mode . turn-on-reftex)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default Tex-master nil)
  (setq reftex-plug-into-AUCTeX t))

(use-package haskell-mode)

(use-package c++-ts-mode
  :ensure nil
  :hook ((c++-ts-mode . chbm-set-c-style-indent)
         (ff-pre-find . chbm-set-cc-search-dirs-project)))

(use-package c-ts-mode
  :ensure nil
  :hook ((c-ts-mode . chbm-set-c-style-indent)
         (ff-pre-find . chbm-set-cc-search-dirs-project)))

(use-package xml-mode
  :ensure nil
  :mode "\\.cts")

(use-package ess
  :hook ((ess-r-mode . (lambda ()
                         (setq-local ess-indent-offset 2
                                     comment-column 0
                                     standard-indent 2
                                     tab-width 2
                                     indent-tabs-mode nil))))
  :config
  (setq ess-use-ido nil))

(use-package cmake-mode
  :ensure nil)

(use-package clang-format
  :ensure nil)

(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.yml")

(use-package zig-mode)

(use-package org
  :ensure nil
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :hook ((org-mode . (lambda ()
                       (add-hook 'completion-at-point-functions #'cape-file nil t)))
         (org-mode . chbm-set-website-config))
  :config
  (require 'org-tempo)
  (require 'ox-publish)
  (setq org-directory "~/documents/org"
        org-agendas-directory (concat org-directory "/agendas/")

        org-icalendar-exclude-tags '("noexport")
        org-icalendar-combined-agenda-file (concat org-directory "/agendas/org.ics")
        org-icalendar-use-scheduled '(event-if-not-todo event-if-todo event-if-todo-not-done todo-start)
        org-icalendar-use-deadline '(event-if-not-todo event-if-todo event-if-todo-not-done todo-due)
        org-icalendar-scheduled-summary-prefix ""
        org-icalendar-deadline-summary-prefix ""
        org-icalendar-timezone "America/Toronto"
        org-icalendar-date-time-format ";TZID=%Z:%Y%m%dT%H%M%S"

        org-babel-load-languages '((C . t) (emacs-lisp . t) (R . t) (shell . t) (python . t))

        org-export-with-toc nil
        org-export-with-section-numbers nil
        org-export-headline-levels 7
        org-export-with-date nil
        org-export-with-author nil

        org-html-validation-link nil

        org-attach-use-inheritance t
        org-attach-auto-tag nil

        org-default-notes-file (expand-file-name (concat org-agendas-directory "a-classer.org") org-directory)
        org-agenda-files (directory-files-recursively org-agendas-directory  "\\.org$")
        org-archive-location (concat org-directory "/archive.org::datetree/")
        org-attach-id-dir (concat org-directory "/data/")
        org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w)" "DONE(d)"))))

(use-package org-download
  :hook ((org-mode . (lambda ()
                       (require 'org-download)))
         (dired-mode . org-download-enable))
  :config
  (setq-default org-download-image-dir "./images"
                org-download-heading-lvl nil))

;;; Abbrevs

(abbrev-table-put global-abbrev-table :regexp "\\(?:^\\|[\t\s]+\\)\\(?1:[:_].*\\|.*\\)")
(define-abbrev global-abbrev-table ":github:" "git@github.com:chabam/")
