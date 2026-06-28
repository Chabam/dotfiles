(setq use-package-always-defer t)
(setq package-quickstart t)
(setq package-install-upgrade-built-in t)
(setq use-package-enable-imenu-support t)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless package-archives
  (package-refresh-contents))

(provide 'external-packages)
