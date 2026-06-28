;; Increase undo values, I make a lot of mistakes
(setq undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000))

(add-hook 'after-init-hook #'delete-selection-mode)

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
(global-set-key (kbd "C-c k p") 'chbm/copy-pwd)
(global-set-key (kbd "C-x r y") 'chbm/yank-copied-rectangle-as-lines)
(global-set-key (kbd "C-S-d") 'duplicate-dwim)
(global-set-key (kbd "M-z") 'zap-up-to-char)       ; zap-up-to-char instead of zap-to-char

(provide 'editing)
