;; -*- lexical-binding: t; -*-

;; Default indentation
(setq-default standard-indent 4
              tab-width 4
              indent-tabs-mode nil)

(defun chbm/local-eglot-workspace-configuration (orig-func server &optional path)
  "Small hack to allow buffer-local `eglot-workspace-configuration' (useful
for non-file buffers)"
  (or (apply orig-func server (and path (list path)))
      (when-let* ((_ path)
                  (buffer (get-file-buffer path)))
        (buffer-local-value 'eglot-workspace-configuration buffer))))

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

  (advice-add 'eglot--workspace-configuration-plist
              :around
              #'chbm/local-eglot-workspace-configuration))

(use-package eglot-inactive-regions
  :after eglot
  :ensure t
  :hook (eglot-connect . (lambda (&rest _)
			   (when (member major-mode '(c++-ts-mode c-ts-mode))
			     (require 'eglot-inactive-regions)
			     (eglot-inactive-regions-mode))))
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

(setopt treesit-enabled-modes t)
(setq treesit-auto-install-grammar 'ask)
(setq-default treesit-font-lock-level 4)

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

(use-package direnv
  :ensure t
  :hook (after-init . direnv-mode))

(use-package gdb-mi
  :ensure nil
  :commands (gdb gud-gdb)
  :config
  (setq gdb-debuginfod-enable-setting nil))

(provide 'chbm-prog)
