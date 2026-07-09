;; Increase undo values, I make a lot of mistakes
(setq undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000))

(add-hook 'after-init-hook #'delete-selection-mode)

(setq sentence-end-double-space nil)

;; Remove blinking for delete-pair
(setq delete-pair-blink-delay 0)

(defun chbm/yank-copied-rectangle-as-lines ()
  "Insert the last copied or killed rectangle as regular lines."
  (interactive)
  (let ((rec (get-text-property 0 'yank-handler (car kill-ring))))
    (when (not rec)
      (user-error "No rectangle to yank"))
    (insert (substring-no-properties (car kill-ring)))))

(use-package electric
  :ensure nil
  :hook ((after-init . electric-pair-mode)))

(use-package paren
  :ensure nil
  :config
  (setq show-paren-when-point-in-periphery t)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-context-when-offscreen 'overlay))

(defun chbm/setup-tempel-capf (&rest _)
  ;; Removing tempel-expand if it was already there first
  (setq-local completion-at-point-functions
              (cons #'tempel-expand (delete #'tempel-expand
                                            completion-at-point-functions))))

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

(defun chbm/buffer-lines-cols-whitespace ()
  (setq show-trailing-whitespace t)
  (column-number-mode)
  (display-line-numbers-mode))

(add-hook 'prog-mode-hook 'chbm/buffer-lines-cols-whitespace)
(add-hook 'text-mode-hook 'chbm/buffer-lines-cols-whitespace)

(global-set-key (kbd "C-M-z") 'delete-pair)
(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "C-c t d") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c /") 'pwd)
(global-set-key (kbd "C-c C-/") 'cd)
(global-set-key (kbd "C-c k /") 'chbm/copy-pwd)
(global-set-key (kbd "C-x r y") 'chbm/yank-copied-rectangle-as-lines)
(global-set-key (kbd "C-S-d") 'duplicate-dwim)
(global-set-key (kbd "M-z") 'zap-up-to-char) ; zap-up-to-char instead of zap-to-char

(global-set-key (kbd "M-'") 'hippie-expand)

;; Useful keys stolen from oantolin
(global-set-key (kbd "C-d") 'delete-forward-char)

(global-set-key (kbd "M-F") 'forward-to-word)
(global-set-key (kbd "M-B") 'backward-to-word)

(global-set-key (kbd "C-M-<") 'mark-beginning-of-buffer)
(global-set-key (kbd "C-M->") 'mark-end-of-buffer)

(global-set-key (kbd "M-T") 'transpose-sentences)

(global-set-key (kbd "C-x M-t") 'transpose-paragraphs)
(global-set-key (kbd "M-H") 'mark-paragraph)
(global-set-key (kbd "M-K") 'kill-paragraph)

(global-set-key (kbd "C-z") 'repeat)

(global-set-key (kbd "M-s f") 'flush-lines)
(global-set-key (kbd "M-s k") 'keep-lines)

(global-set-key (kbd "C-M-o") 'up-list)
(global-set-key (kbd "M-R") 'kill-backward-up-list)

(provide 'editing)
