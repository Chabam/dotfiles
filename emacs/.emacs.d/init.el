(add-to-list 'load-path (expand-file-name "configs" user-emacs-directory))

;; Order is important here
(require 'external-packages)
(require 'basic)
(require 'containerized)
(require 'appearance)
(require 'frames-and-windows)
(require 'minibuf)
