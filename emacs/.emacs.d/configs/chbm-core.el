;; no startup screen
(setq inhibit-startup-screen t)

(setq initial-scratch-message nil)

(setq mouse-yank-at-point t)
(setq mouse-drag-copy-region t)

;; No bells
(setq visible-bell nil)
(setq ring-bell-function #'ignore)

;; Be concise
(setq use-short-answers t)

;; No limit for printing lisp values
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

(setq read-process-output-max (* 4 1024 1024))

;; I know how to work an Emacs client, thanks
(setq server-client-instructions nil)

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

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :config
  (setq dired-listing-switches "-aBhlv  --group-directories-first")
  (setq dired-dwim-target t)
  (setq dired-vc-rename-file t)
  (setq dired-vc-rename-file t)
  (setq delete-by-moving-to-trash t)
  (setq wdired-allow-to-change-permissions t)

  (when chbm/emacs-containerized
    (define-key dired-mode-map (kbd "E") #'chbm/dired-do-open-containerized)))

(use-package compat
  :ensure t)

(use-package browse-url
  :if chbm/emacs-containerized
  :config
  (setq browse-url-browser-function
        (lambda (url &optional _)
          (start-process "browse-url-browser" nil "flatpak-xdg-open" url))))

;; Must be before the docker package!!
(when chbm/emacs-containerized
  (setq tramp-podman-program "podman-remote"))

(use-package docker
  :ensure t
  :commands (docker docker-compose)
  :bind (("C-c p" . docker)) ;; `p' because podman :)
  :config
  (setq docker-command (if chbm/emacs-containerized
                           "podman-remote"
                         "podman"))
  (setq docker-compose-command (if chbm/emacs-containerized
                                   "podman-remote compose"
                                 "podman compose")))

;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
(setq remote-file-name-inhibit-locks t)
(setq tramp-use-scp-direct-remote-copying t)
(setq remote-file-name-inhibit-auto-save-visited t)
(setq tramp-copy-size-limit (* 1024 1024)) ;; 1mb
(setq remote-file-name-inhibit-cache 50)
(setq remote-file-name-inhibit-locks t)
(setq remote-file-name-inhibit-delete-by-moving-to-trash t)
(setq tramp-verbose 1)

(with-eval-after-load 'tramp
  ;; Inspired by: https://samsai.eu/post/toolbox-based-emacs-flatpak-workflow/
  (add-to-list 'tramp-methods
	           (cons
		        "toolbox"
		        '((tramp-login-program "toolbox")
		          (tramp-login-args (("enter" "-c") ("%h")))
		          (tramp-remote-shell "/bin/sh")
		          (tramp-remote-shell-args ("-c")))))

  (defun chbm/tramp-toolbox-completion-function (&rest _)
    "Returns a list of available Toolbox containers"
    (tramp-skeleton-completion-function "toolbox"
      (when-let* ((raw-list
		           (shell-command-to-string
		            (concat "toolbox" " list -c")))
		          (lines (split-string raw-list "\n" 'omit))
		          (names
		           (tramp-compat-seq-keep
		            (lambda (line)
		              (when (string-match
			                 "^[a-z0-9]+[[:blank:]]*\\(.*?\\)[[:blank:]]"
			                 line)
		                (or (match-string 2 line) (match-string 1 line))))
		            (cdr lines))))
        (mapcar (lambda (name) (list nil name)) names))))

  (tramp-set-completion-function
   "toolbox"
   '((chbm/tramp-toolbox-completion-function "")))

  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process))

(with-eval-after-load 'compile
  (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options))


(provide 'chbm-core)
