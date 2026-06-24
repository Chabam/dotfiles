;; no startup screen
(setq inhibit-startup-screen t)

;; No bells
(setq visible-bell nil)
(setq ring-bell-function #'ignore)

;; Be concise
(setq use-short-answers t)

;; No limit for printing lisp values
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

(setq read-process-output-max (* 4 1024 1024))

;; custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load-file custom-file)
  (write-region "" nil custom-file))

(use-package no-littering
  :demand t
  :ensure t
  :config
  (no-littering-theme-backups)
  (let ((dir (no-littering-expand-var-file-name "lock-files/")))
    (make-directory dir t)
    (setq lock-file-name-transforms `((".*" ,dir t)))))

(provide 'basic)
