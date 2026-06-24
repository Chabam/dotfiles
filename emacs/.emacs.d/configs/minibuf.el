(setq enable-recursive-minibuffers t)
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(ido-mode 1)

(provide 'minibuf)
