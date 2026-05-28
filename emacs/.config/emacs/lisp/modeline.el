;; Most stuff here comes from Prot

(defface chbm/modeline-green-bg
  '((t ()))
  "Face for modeline indicators with a background."
  :group 'chbm/modeline-faces)

(defun chbm/update-modeline-colors ()
  (modus-themes-with-colors
    (face-spec-set 'chbm/modeline-green-bg
                   `((t :inherit bold :foreground ,green :background ,bg-green-intense :box ,green)))
    (face-spec-set 'chbm/modeline-magenta-bg
                   `((t :inherit bold :foreground ,magenta :background ,bg-magenta-intense :box ,magenta)))
    (face-spec-set 'chbm/modeline-red-bg
                   `((t :inherit bold :foreground ,red :background ,bg-red-intense :box ,red)))
    (face-spec-set 'chbm/modeline-cyan-bg
                   `((t :inherit bold :foreground ,cyan :background ,bg-cyan-intense :box ,cyan)))
    (face-spec-set 'chbm/modeline-red-fg
                   `((t :foreground ,red)))))


(defface chbm/modeline-magenta-bg
  '((t ()))
  "Face for modeline indicators with a background."
  :group 'chbm/modeline-faces)

(defface chbm/modeline-red-bg
  '((t ()))
  "Face for modeline indicators with a background."
  :group 'chbm/modeline-faces)

(defface chbm/modeline-red-fg
  '((t ()))
  "Face for modeline indicators with a background."
  :group 'chbm/modeline-faces)

(defface chbm/modeline-cyan-bg
  '((t ()))
  "Face for modeline indicators with a background."
  :group 'chbm/modeline-faces)

(defvar-local chbm/modeline-kbd-macro
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
        (propertize " KMacro " 'face 'chbm/modeline-green-bg)))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")

(defun chbm/modeline-buffer-name ()
  "Return buffer name, with read-only indicator if relevant."
  (let ((name (buffer-name)))
    (if buffer-read-only
        (format "%s %s" (char-to-string #xE0A2) name)
      name)))

(defun chbm/modeline-buffer-identification-face ()
  "Return appropriate face or face list for `chbm/modeline-buffer-identification'."
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

(defvar-local chbm/modeline-narrow
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (buffer-narrowed-p)
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        (propertize " Narrow " 'face 'chbm/modeline-cyan-bg)))
  "Mode line construct to report the narrowed state of the current buffer.")

(defvar-local chbm/modeline-buffer-identification
    '(:eval
      (propertize (chbm/modeline-buffer-name)
                  'face (chbm/modeline-buffer-identification-face)
                  'mouse-face 'mode-line-highlight
                  'help-echo (chbm/modeline-buffer-name-help-echo)))
  "Mode line construct for identifying the buffer being displayed.
Propertize the current buffer with the `mode-line-buffer-id'
face.  Let other buffers have no face.")

(defun chbm/modeline-buffer-name-help-echo ()
  "Return `help-echo' value for `chbm/modeline-buffer-identification'."
  (concat
   (propertize (buffer-name) 'face 'mode-line-buffer-id)
   "\n"
   (propertize
    (or (buffer-file-name)
        (format "No underlying file.\nDirectory is: %s" default-directory))
    'face 'font-lock-doc-face)))

(defvar-local chbm/modeline-remote-status
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (file-remote-p default-directory))
        (propertize " @ "
                    'face 'chbm/modeline-magenta-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct for showing remote file name.")

(defvar-local chbm/modeline-window-dedicated-status
    '(:eval
      (when
          (and (mode-line-window-selected-p)
               (window-dedicated-p))
        (propertize " = "
                    'face 'chbm/modeline-red-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct for dedicated window indicator.")

(defun chbm/modeline-major-mode-indicator ()
  "Return appropriate propertized mode line indicator for the major mode."
  (let ((indicator (cond
                    ((derived-mode-p 'text-mode) "§")
                    ((derived-mode-p 'prog-mode) "λ")
                    ((derived-mode-p 'comint-mode) ">_")
                    (t "◦"))))
    (propertize indicator 'face 'shadow)))

(defun chbm/modeline-major-mode-name ()
  "Return capitalized `major-mode' without the -mode suffix."
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))

(defun chbm/modeline-major-mode-help-echo ()
  "Return `help-echo' value for `chbm/modeline-major-mode'."
  (if-let* ((parent (get major-mode 'derived-mode-parent)))
      (format "Symbol: `%s'.  Derived from: `%s'" major-mode parent)
    (format "Symbol: `%s'." major-mode)))

(defvar-local chbm/modeline-major-mode
    (list
     (propertize "%[" 'face 'chbm/modeline-red-fg)
     '(:eval
       (propertize (concat
                    (chbm/modeline-major-mode-indicator)
                    " "
                    (chbm/modeline-major-mode-name))
                   'mouse-face 'mode-line-highlight
                   'help-echo (chbm/modeline-major-mode-help-echo)))
     (propertize "%]" 'face 'chbm/modeline-red-fg))
  "Mode line construct for displaying major modes.")

(declare-function vc-git--symbolic-ref "vc-git" (file))

(defun chbm/modeline--vc-branch-name (file backend)
  "Return capitalized VC branch name for FILE with BACKEND."
  (when-let* ((rev (vc-working-revision file backend))
              (branch (or (vc-git--symbolic-ref file)
                          (substring rev 0 7))))
    branch))

(declare-function vc-git-working-revision "vc-git" (file))

(defvar chbm/modeline-vc-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'vc-diff)
    (define-key map [mode-line down-mouse-3] 'vc-root-diff)
    map)
  "Keymap to display on VC indicator.")

(defun chbm/modeline--vc-help-echo (file)
  "Return `help-echo' message for FILE tracked by VC."
  (format "Revision: %s\nmouse-1: `vc-diff'\nmouse-3: `vc-root-diff'"
          (vc-working-revision file)))

(defun chbm/modeline--vc-text (file branch &optional face)
  "Prepare text for Git controlled FILE, given BRANCH.
With optional FACE, use it to propertize the BRANCH."
  (concat
   (propertize (char-to-string #xE0A0) 'face 'shadow)
   " "
   (propertize branch
               'face face
               'mouse-face 'mode-line-highlight
               'help-echo (chbm/modeline--vc-help-echo file)
               'local-map chbm/modeline-vc-map)))

(defun chbm/modeline--vc-details (file branch &optional face)
  "Return Git BRANCH details for FILE, truncating it if necessary.
The string is truncated if the width of the window is smaller
than `split-width-threshold'."
  (chbm/modeline--vc-text file branch face))

(defvar chbm/modeline--vc-faces
  '((added . vc-locally-added-state)
    (edited . vc-edited-state)
    (removed . vc-removed-state)
    (missing . vc-missing-state)
    (conflict . vc-conflict-state)
    (locked . vc-locked-state)
    (up-to-date . vc-up-to-date-state))
  "VC state faces.")

(defun chbm/modeline--vc-get-face (key)
  "Get face from KEY in `chbm/modeline--vc-faces'."
  (alist-get key chbm/modeline--vc-faces 'vc-up-to-date-state))

(defun chbm/modeline--vc-face (file backend)
  "Return VC state face for FILE with BACKEND."
  (when-let ((key (vc-state file backend)))
    (chbm/modeline--vc-get-face key)))

(defvar-local chbm/modeline-vc-branch
    '(:eval
      (when-let* (((mode-line-window-selected-p))
                  (file (or buffer-file-name default-directory))
                  (backend (or (vc-backend file) 'Git))
                  (branch (chbm/modeline--vc-branch-name file backend))
                  (face (chbm/modeline--vc-face file backend)))
        (chbm/modeline--vc-details file branch face)))
  "Mode line construct to return propertized VC branch.")

(defvar-local chbm/modeline-diagnostics
    '(:eval
      (when-let* (((mode-line-window-selected-p))
                  (eglot '(eglot--managed-mode ("[" eglot-mode-line-format "]")))
                  (flymake (if (bound-and-true-p flymake-mode)
                               '(flymake-mode-line-exception flymake-mode-line-counters)
                             nil)))
        (list eglot flymake))
      "Mode line construct to return propertized buffer diagnostics."))


(dolist (construct '(chbm/modeline-kbd-macro
                     chbm/modeline-narrow
                     chbm/modeline-remote-status
                     chbm/modeline-buffer-identification
                     chbm/modeline-window-dedicated-status
                     chbm/modeline-major-mode
                     chbm/modeline-vc-branch
                     chbm/modeline-diagnostics
                     ))
  (put construct 'risky-local-variable t))

(setq-default mode-line-format
              '("%e"
                chbm/modeline-kbd-macro
                chbm/modeline-narrow
                chbm/modeline-remote-status
                chbm/modeline-window-dedicated-status
                "  "
                chbm/modeline-buffer-identification
                "  "
                chbm/modeline-major-mode
                mode-line-process

                mode-line-format-right-align

                chbm/modeline-diagnostics
                "  "
                chbm/modeline-vc-branch
                " %p %l:%c  ")
              )

(add-hook #'auto-dark-dark-mode-hook 'chbm/update-modeline-colors)
(add-hook #'auto-dark-light-mode-hook 'chbm/update-modeline-colors)
