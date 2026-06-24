(add-to-list 'load-path (expand-file-name "configs" user-emacs-directory))

;; Order is important here
(require 'basic)
(require 'containerized)
(require 'external-packages)
(require 'appearance)
(require 'frames-and-windows)
(require 'minibuf)
