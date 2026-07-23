(let ((load-path (append load-path
			 (list (expand-file-name "configs" user-emacs-directory)))))

  ;; Order is important here
  (require 'chbm-packages)
  (require 'chbm-containerized)

  (require 'chbm-appearance)
  (require 'chbm-core)
  (require 'chbm-frames-and-windows)

  (require 'chbm-minibuffer)
  (require 'chbm-searching)
  (require 'chbm-completion)

  (require 'chbm-keybindings)
  (require 'chbm-editing)
  (require 'chbm-abbrev)

  (require 'chbm-projects)
  (require 'chbm-prog)
  (require 'chbm-prog-modes)
  (require 'chbm-comint-modes)

  (require 'chbm-writing)
  (require 'chbm-org)

  ;; If I ever get forced to use this proprietary garbage
  (when (eq system-type 'windows-nt)
    (require 'chbm-ms-windows)))
