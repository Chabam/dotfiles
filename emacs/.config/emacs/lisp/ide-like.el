;; LSPs
(use-package eglot
  :ensure nil
  :bind (("C-c e a" . eglot-code-actions)
         ("C-c e r" . eglot-rename)
         ("C-c e s" . eglot))
  :config
  (setq eglot-autoshutdown t
        eglot-stay-out-of '("yasnippet") ; Might activate later
        eglot-ignored-server-capabilities (append eglot-ignored-server-capabilities
                                                  '(:inlayHintProvider
                                                    :documentOnTypeFormattingProvider
                                                    :documentOnTypeFormatting))
        eglot-send-changes-idle-time 0.6
        eglot-sync-connect nil
        eglot-events-buffer-config '(:size 0 :format full))
  (add-to-list 'eglot-server-programs
               '((org-mode markdown-mode (LaTeX-mode :language-id "latex")) . ("ltex-ls-plus")))
  (add-to-list 'eglot-server-programs
               '((c-ts-mode c++-ts-mode) . ("clangd"
                                            "--header-insertion=never"
                                            "--completion-style=detailed"
                                            "--clang-tidy=false"
                                            "--log=error"
                                            "--background-index"))))

(use-package eglot-inactive-regions
  :hook (after-init . (lambda () (eglot-inactive-regions-mode 1)))
  :config
  (setq eglot-inactive-regions-style 'darken-foreground)
  (setq eglot-inactive-regions-opacity 0.4))

(use-package flymake
  :ensure nil
  :bind (("C-c f d" . flymake-show-buffer-diagnostics)
         ("C-c f D" . flymake-show-project-diagnostics)
         ("C-c f n" . flymake-goto-next-error)
         ("C-c f p" . flymake-goto-prev-error))
  :config
  (setq flymake-no-changes-timeout 1.0))

;; Debugs

(use-package dape
  :config
  (setq dape-buffer-window-arrangement 'right))

;; Syntax highligthing

(use-package treesit-auto
  :hook (after-init . (lambda () (global-treesit-auto-mode 1)))
  :config
  (setq treesit-auto-install 'prompt)
  (setq treesit-auto-add-to-auto-mode-alist 'all)
  (delete 'glsl treesit-auto-langs)
  (setq-default treesit-font-lock-level 4))

;; Auto completion

(use-package corfu
  :bind (:map corfu-map
              ("RET" . nil))
  :hook ((inferior-python-mode . (lambda () (corfu-auto nil)))
         (after-init . global-corfu-mode)
         (after-init . corfu-popupinfo-mode))
  :config
  (setq corfu-auto t)
  (setq corfu-auto-prefix 3)
  (setq corfu-auto-delay 0.3)
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

;; Snippets

(use-package yasnippet
  :hook ((after-init . (lambda () (yas-global-mode 1))))
  :config
  (setq yas-snippet-dirs (list
                          (expand-file-name "snippets" user-emacs-directory))))

(use-package find-file
  :ensure nil
  :bind (("C-c o" . chbm/ff-find-other-file)))

(defun chbm/recompile-dwim (&optional display-buf)
  "Recompile using the last compilation-mode buffer"
  (interactive "P")
  (let* ((default-buf-name "*compilation*")
         (buf-name (if (project-current)
                       (project-prefixed-buffer-name "compilation")
                     default-buf-name))
         (comp-buf (get-buffer buf-name)))
    ;; If the compilation buffer for the project is not found, use the
    ;; default one instead
    (when (and (not comp-buf)
               (project-current))
      (setq comp-buf (get-buffer default-buf-name)))
    (if (and comp-buf (buffer-live-p comp-buf))
        (with-current-buffer comp-buf
          (if display-buf
              (recompile)
            (let ((display-buffer-alist
                   `((,(replace-regexp-in-string "\\*" "\\\\*" buf-name) (display-buffer-no-window)))))
              (recompile))))
      (message "No active compilation buffer found."))))

(use-package compile
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter)
  :bind (("C-x M-c" . compile)
         ("<f5>" . chbm/recompile-dwim))
  :config
  (setq compilation-max-output-line-length nil)
  (setq ansi-color-for-compilation-mode t)
  (setq compilation-scroll-output 'first-error)
  (setq compilation-skip-threshold 2))
