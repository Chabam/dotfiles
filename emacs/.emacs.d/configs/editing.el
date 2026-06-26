;; Increase undo values, I make a lot of mistakes
(setq undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000))

(delete-selection-mode 1)

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

(provide 'editing)
