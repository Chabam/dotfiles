(setq enable-recursive-minibuffers t)

(add-hook 'after-init-hook #'minibuffer-depth-indicate-mode)
(add-hook 'after-init-hook #'minibuffer-electric-default-mode)

(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-count 10))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(provide 'minibuf)
