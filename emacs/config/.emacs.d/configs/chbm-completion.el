;; -*- lexical-binding: t; -*-

(defcustom chbm/modern-completion t
  "Whether or not to use the \"modern\" Emacs completion stack (`vertico',
`corfu', etc)"
  :type 'boolean)

(defun chbm/silent-truncate-lines ()
  (setq truncate-lines t))

(use-package minibuffer
  :unless chbm/modern-completion
  :ensure nil
  :bind (:map minibuffer-visible-completions-up-down-map
		  ("C-n" . minibuffer-next-completion)
		  ("C-p" . minibuffer-previous-completion))
  :hook ((minibuffer-setup . cursor-intangible-mode)
         (completion-list-mode . chbm/silent-truncate-lines)
		 (minibuffer-setup . chbm/silent-truncate-lines))
  :config
  (setq completions-detailed t)
  (setq tab-always-indent 'complete)
  (setq completion-auto-help t)
  (setq completion-auto-select 'second-tab)
  (setq completion-eager-update t)
  (setq completion-eager-display t)
  (setq minibuffer-visible-completions 'up-down)
  (setq completion-ignore-case t)
  (setq completion-show-help nil)
  (setq completions-format 'one-column)
  (setq completions-max-height 12)
  (setq completions-sort 'historical)
  (setq enable-recursive-minibuffers t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-minibuffer-restore-windows nil)
  (setq minibuffer-prompt-properties
   '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  (setq minibuffer-depth-indicate-mode t)
  (setq minibuffer-electric-default-mode t)
  (setq completion-styles '(basic substring partial-completion))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles . (basic partial-completion)))
                                        (buffer (styles . (substring)))
                                        (project-file (styles . (substring partial-completion))))))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure t
  :config
  (add-to-list 'completion-styles 'orderless)
  (mapc (lambda (cat)
          (let ((completions (assq 'styles (cdr cat))))
            (unless (member 'orderless (cdr completions))
              (setcdr completions (append (cdr completions) '(orderless))))))
        completion-category-overrides)
  (setq orderless-matching-styles '(orderless-literal orderless-regexp)))

(defun chbm/capf-prog-mode ()
  (dolist (fn '(cape-keyword
                cape-file
                cape-dabbrev))
    (add-hook 'completion-at-point-functions
              fn
              'append
              'local)))

(defun chbm/capf-text-mode ()
  (dolist (fn '(cape-file
                cape-line
                cape-dabbrev))
    (add-hook 'completion-at-point-functions
              fn
              'append
              'local)))

(use-package cape
  :ensure t
  :hook ((prog-mode . chbm/capf-prog-mode)
         (text-mode . chbm/capf-text-mode)))

(use-package vertico
  :if chbm/modern-completion
  :ensure t
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-count 10))

(use-package corfu
  :if chbm/modern-completion
  :ensure t
  :bind (:map corfu-map
          ("RET" . nil))
  :hook ((after-init . global-corfu-mode)
	     (after-init . corfu-popupinfo-mode))
  :config
  (setq corfu-cycle t)
  (setq corfu-popupinfo-delay 0.5)
  (setq tab-always-indent 'complete))

(defun chbm/completion-preview-only-local-mode ()
  (if (file-remote-p default-directory)
      (completion-preview-mode -1)
    (completion-preview-mode 1)))

(use-package completion-preview
  :hook ((after-init . global-completion-preview-mode)
         (minibuffer-mode . chbm/completion-preview-only-local-mode)
         (minibuffer-inactive-mode . chbm/completion-preview-only-local-mode)
         (eshell-mode . chbm/completion-preview-only-local-mode)
         (eshell-directory-change . chbm/completion-preview-only-local-mode))
  :bind (:map completion-preview-active-mode-map
              ("C-," . completion-preview-next-candidate)
              ("C-." . completion-preview-prev-candidate))
  :config
  (setq completion-preview-ignore-case t)
  (setq completion-preview-idle-delay 0.2))

(defun chbm/setup-tempel-capf (&rest _)
  (setq-local corfu-auto-trigger "~")
  (add-hook 'completion-at-point-functions
            (cape-capf-trigger #'tempel-complete ?~)
            nil
            'local))

(use-package tempel
  :ensure t
  :bind ((:map tempel-map
               ("<tab>" . tempel-next)
               ("<backtab>" . tempel-previous)))
  :hook ((eglot-managed-mode . chbm/setup-tempel-capf)
         (prog-mode . chbm/setup-tempel-capf)
         (org-mode . chbm/setup-tempel-capf))
  :config
  (setq tempel-path (expand-file-name "templates" user-emacs-directory)))

(provide 'chbm-completion)
