(use-package orderless
  :ensure t
  :config
  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq completion-styles '(basic orderless))
  (setq orderless-matching-styles '(orderless-literal orderless-regexp))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles . (basic partial-completion orderless)))
                                        (buffer (styles . (substring orderless)))
                                        (project-file (styles . (substring orderless)))))
  (setq completions-sort 'history))


(defun chbm/capf-prog-mode ()
  (add-hook 'completion-at-point-functions
            (cape-capf-super #'cape-dabbrev
                             #'cape-file
                             #'cape-keyword)
            'append
            'local))

(defun chbm/capf-text-mode ()
  (add-hook 'completion-at-point-functions
            (cape-capf-super #'cape-dabbrev
                             #'cape-line
                             #'cape-dict
                             #'cape-file)
            'append
            'local))

(use-package cape
  :ensure t
  :hook ((prog-mode . chbm/capf-prog-mode)
         (text-mode . chbm/capf-text-mode)))

(use-package corfu
  :ensure t
  :bind (:map corfu-map
          ("RET" . nil))
  :hook ((after-init . global-corfu-mode)
	     (after-init . corfu-popupinfo-mode))
  :config
  (setq corfu-cycle t)
  (setq corfu-popupinfo-delay 0.5)
  (setq tab-always-indent 'complete))

;; Completion stuff
(setq text-mode-ispell-word-completion nil)

(defun chbm/completion-preview-only-local-mode ()
  (if (file-remote-p default-directory)
      (completion-preview-mode -1)
    (completion-preview-mode 1)))

(use-package completion-preview
  :hook ((prog-mode . completion-preview-mode)
         (text-mode . completion-preview-mode)
         (comint-mode . completion-preview-mode)
         (eshell-mode . chbm/completion-preview-only-local-mode)
         (eshell-directory-change . chbm/completion-preview-only-local-mode))
  :bind (:map completion-preview-active-mode-map
              ("M-n" . completion-preview-next-candidate)
              ("M-p" . completion-preview-previous-candidate)))

(defun chbm/setup-tempel-capf (&rest _)
  ;; Removing tempel-expand if it was already there first
  (setq-local corfu-auto-trigger "/")
  (add-hook 'completion-at-point-functions
            (cape-capf-trigger #'tempel-complete ?/)
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
