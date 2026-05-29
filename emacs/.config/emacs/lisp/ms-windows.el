(when (member "Segoe UI Emoji" (font-family-list))
  (set-fontset-font t 'emoji (font-spec :family "Segoe UI Emoji") nil 'prepend))

(setq find-program "C:\\msys64\\usr\\bin\\find.exe")
