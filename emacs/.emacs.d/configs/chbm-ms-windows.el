;; Needed for some reason for fixing server mode on Windows
(setq server-auth-dir (file-name-concat user-emacs-directory "server"))
(unless (file-exists-p server-auth-dir)
  (make-directory server-auth-dir t))

(when (member "Segoe UI Emoji" (font-family-list))
  (set-fontset-font t 'emoji (font-spec :family "Segoe UI Emoji") nil 'prepend))

;; Must install msys64!
(setq find-program "C:\\msys64\\usr\\bin\\find.exe")

(provide 'chbm-ms-windows)
