(defcustom chbm/emacs-containerized
  nil
  "Sets some stuff so that this Emacs works under a container (most likely
   a toolbox container)"
  :type 'boolean)

(defun chbm/xdg-open-host (file &rest _)
  (call-process "flatpak-spawn" nil 0 nil "--host" "xdg-open" file))

(defun chbm/dired-do-open-containerized (&optional arg)
  "Does the same as `dired-do-open' but approprietly
calls `flatpak-spawn --host xdg-open'"
  (interactive "P" dired-mode)
  (let ((files (if (mouse-event-p last-nonmenu-event)
                   (save-excursion
                     (mouse-set-point last-nonmenu-event)
                     (dired-get-marked-files nil arg))
                 (dired-get-marked-files nil arg))))

    ;; This is never going to be on other system than GNU/Linux, so no
    ;; need to do the same gymnastic as the original `dired-do-open'
    (mapc #'chbm/xdg-open-host files)))

(when chbm/emacs-containerized
  (setq browse-url-browser-function
        (lambda (url &optional _)
          (start-process "browse-url-browser" nil "flatpak-xdg-open" url))))

(provide 'containerized)
