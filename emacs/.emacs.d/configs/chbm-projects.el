(setq delete-by-moving-to-trash t)
(setq vc-follow-symlinks t)

(add-hook 'log-edit-mode-hook (lambda ()
                                (display-line-numbers-mode -1)))

(setq project-switch-commands
      '((project-find-file "Find file")
        (project-find-regexp "Find regexp")
        (project-find-dir "Find directory")
        (project-dired "Project Dired" "D")
        (project-vc-dir "VC-Dir")
        (magit-project-status "Magit" "m")
        (project-eshell "Eshell")
        (project-any-command "Other")))
(setq project-mode-line t)
(setq project-compilation-buffer-name-function 'project-prefixed-buffer-name)

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-keep-variants nil)
(setq ediff-make-buffers-readonly-at-startup nil)
(setq ediff-merge-revisions-with-ancestor t)
(setq ediff-show-clashes-only t)

(use-package magit
  :ensure t
  :commands (magit magit-status)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  ;; For tramp
  (setq magit-tramp-pipe-stty-settings 'pty))

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

(provide 'chbm-projects)
