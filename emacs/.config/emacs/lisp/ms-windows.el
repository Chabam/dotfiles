;; Needed for some reason
(setq server-auth-dir (file-name-concat user-emacs-directory "server"))
(unless (file-exists-p server-auth-dir)
  (make-directory server-auth-dir t))

(when (member "Segoe UI Emoji" (font-family-list))
  (set-fontset-font t 'emoji (font-spec :family "Segoe UI Emoji") nil 'prepend))

(setq find-program "C:\\msys64\\usr\\bin\\find.exe")
