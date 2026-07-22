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
                                        (buffer (styles . (substring orderless)))))
  (setq completions-sort 'history))


(defun chbm/capf-prog-mode ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super #'cape-dabbrev
                                     #'cape-abbrev
                                     #'cape-file
                                     #'cape-keyword))))

(defun chbm/capf-text-mode ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super #'cape-dabbrev
                                     #'cape-abbrev
                                     #'cape-line
                                     #'cape-dict
                                     #'cape-file))))
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

(use-package completion-preview
  :hook ((prog-mode . completion-preview-mode)
         (text-mode . completion-preview-mode)
         (comint-mode . completion-preview-mode))
  :bind (:map completion-preview-active-mode-map
              ("M-n" . completion-preview-next-candidate)
              ("M-p" . completion-preview-previous-candidate)))

(defun chbm/setup-tempel-capf (&rest _)
  ;; Removing tempel-expand if it was already there first
  (setq-local corfu-auto-trigger "/")
  (setq-local completion-at-point-functions
              (cons (cape-capf-trigger #'tempel-complete ?/)
                    completion-at-point-functions)))

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
