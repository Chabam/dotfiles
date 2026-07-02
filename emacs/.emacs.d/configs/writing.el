(use-package jinx
  :hook ((org-mode . jinx-mode)
         (text-mode . jinx-mode)
         (markdown-mode . jinx-mode))
  :ensure t
  :config
  (setq jinx-languages "en_CA fr_CA"))

;; Thanks Prot!
(defun prot-spell-ispell-display-buffer (buffer)
  "Function to override `ispell-display-buffer' for BUFFER.
Use this as `advice-add' to override the aforementioned Ispell
function.  Then you can control the buffer's specifics via
`display-buffer-alist' (how it ought to be!)."
  (pop-to-buffer buffer)
  (set-window-point (get-buffer-window buffer) (point-min)))

(advice-add #'ispell-display-buffer :override #'prot-spell-ispell-display-buffer)


(provide 'writing)
