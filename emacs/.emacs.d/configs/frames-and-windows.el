(defun chbm/move-window-in-direction (dir)
  (condition-case error
      (windmove-swap-states-in-direction dir)
    (user-error
     (let* ((sel-win (selected-window))
            (root-win (frame-root-window))
            (sel-win-par (window-parent sel-win)))
       (if (eq root-win sel-win-par)
           (user-error (concat "Cannot move window "
                               (symbol-name dir)
                               ", window is alread at the edge"))
         (let* ((width (window-total-width sel-win))
                (height (window-total-height sel-win))
                (new-win (split-window (window-parent sel-win) nil dir)))
           (delete-window sel-win)
           (select-window new-win)))))))

(defun chbm/move-window-right ()
  (interactive)
  (chbm/move-window-in-direction 'right))

(defun chbm/move-window-left ()
  (interactive)
  (chbm/move-window-in-direction 'left))

(defun chbm/move-window-above ()
  (interactive)
  (chbm/move-window-in-direction 'above))

(defun chbm/move-window-below ()
  (interactive)
  (chbm/move-window-in-direction 'below))

(use-package transpose-frame
  :ensure t
  :bind (("C-x 5 t" . transpose-frame)))

(use-package windmove
  :ensure nil
  :hook (after-init . windmove-mode)
  :bind (("C-S-<right>" . chbm/move-window-right)
         ("C-S-<left>" . chbm/move-window-left)
         ("C-S-<up>" . chbm/move-window-above)
         ("C-S-<down>" . chbm/move-window-below))
  :config
  (windmove-default-keybindings 'ctrl)
  ;; (windmove-swap-states-default-keybindings '(ctrl shift))
  (windmove-delete-default-keybindings))

(use-package undelete-frame-mode
  :ensure nil
  :hook (after-init . undelete-frame-mode))

(setq frame-resize-pixelwise t)

(provide 'frames-and-windows)
