(add-to-list 'load-path (concat user-emacs-directory "lisp") t)

(load "theming")
(load "containerized")
(load "core")
(load "frame-and-windows")
(load "minibuffer-enhancements")
(load "editor-enhancements")
(load "source-control")
(load "ide-like")
(load "interactive-modes")
(load "prog-modes")
(load "org-and-friends")
(load "abbreviations")

;; If I ever get forced to use this proprietary garbage
(when (eq system-type 'windows-nt)
  (load "ms-windows"))
