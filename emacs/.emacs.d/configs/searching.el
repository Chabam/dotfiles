(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-additional-variables
        '(register-alist kill-ring
          mark-ring global-mark-ring
          search-ring regexp-search-ring)))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-max-saved-items 300)
  (setq recentf-max-menu-items 15))

(use-package saveplace
  :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook (after-init . save-place-mode)
  :init
  (setq save-place-limit 400))

(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-fringe-mark nil)
  (setq bookmark-save-flag 1))

(use-package orderless
  :ensure t
  :config
  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles basic partial-completion))))
  (setq completion-matching-styles '(orderless-regexp)))

(use-package wgrep
  :ensure t)

(use-package isearch
  :ensure nil
  :config
  (setq isearch-lazy-count t)
  (setq isearch-lazy-count-prefix-format "%s/%s")
  (setq isearch-lazy-count-suffix-format nil))

(provide 'searching)
