(let ((load-path (append load-path
			 (list (expand-file-name "configs" user-emacs-directory)))))

  ;; Order is important here
  (require 'external-packages)
  (require 'containerized)

  (require 'appearance)
  (require 'basic)
  (require 'frames-and-windows)

  (require 'minibuf)
  (require 'searching)

  (require 'keybinds)
  (require 'editing)
  (require 'abbreviations)

  (require 'project-management)
  (require 'programming)
  (require 'programming-languages)
  (require 'interactive-modes)

  (require 'writing)
  (require 'org-and-friends)

  ;; If I ever get forced to use this proprietary garbage
  (when (eq system-type 'windows-nt)
    (require 'ms-windows)))
