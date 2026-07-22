(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

(setq package-archive-priorities
       '(("gnu" . 3)
         ("nongnu" . 2)
         ("melpa" . 1)))

(setq use-package-enable-imenu-support t)
(setq use-package-compute-statistics t)

;; On server mode I don't care about initial loading time
(setq use-package-always-demand (daemonp))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(provide 'external-packages)
