(use-package ediff
  :ensure nil
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor t)
  (setq ediff-show-clashes-only t))

(use-package magit
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Retiring this because it gets pretty slow
;; (use-package diff-hl
;;   :hook ((dired-mode . diff-hl-dired-mode)
;;          (magit-post-refresh . diff-hl-magit-post-refresh)
;;          (after-init . global-diff-hl-mode)
;;          (after-init . diff-hl-flydiff-mode))
;;   :config
;;   (setq diff-hl-update-async 'thread)
;;   (setq diff-hl-flydiff-delay 0.5)
;;   (setq diff-hl-global-modes '(not
;; 			                   image-mode
;; 			                   pdf-view-mode
;; 			                   nov-mode)))

