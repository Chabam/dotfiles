(setq package-quickstart t)
(setq package-install-upgrade-built-in t)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless package-archives
  (package-refresh-contents))

(provide 'external-packages)
