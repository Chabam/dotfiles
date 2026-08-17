;; -*- lexical-binding: t; -*-

(require 'xref)

(defvar-local chbm/associated-container nil
  "Defines the name of the associated container that should run various
commands")

(defvar-local chbm/associated-container-engine "toolbox"
  "The backend of the associated container that should run
 various commands. Supported values are:
  - toolbox
  - distrobox
  - docker
  - podman
  - podman-remote
For the best experience, use toolbox/distrobox!
")

(defun chbm/toolbox-integration--should-forward-p ()
  (and chbm/toolbox-integration-mode
       chbm/associated-container))

(defun chbm/toolbox-integration--generate-cmd-prefix (&optional interactive)
  (let ((engine chbm/associated-container-engine))
    (append (list engine)
            (cond
             ((chbm/toolbox-integration--box-style-engine-p) '("run" "-c"))
             ;; TODO: would be nice if we could dynamically choose
             ;; between `exec' and `run' if the container is running
             ;; or not
             ((chbm/toolbox-integration--docker-style-engine-p) `("exec" ,(when interactive "-i")))
             (t (error (format "'%s' engine is not recognized." engine))))
            (list chbm/associated-container))))

(defun chbm/toolbox-integration--box-style-engine-p ()
  (member chbm/associated-container-engine '("toolbox" "distrobox")))

(defun chbm/toolbox-integration--docker-style-engine-p ()
  (member chbm/associated-container-engine '("podman" "podman-remote" "docker")))

(defun chbm/toolbox-integration--set-to-container-lsp (lsp-val)
  (when (and (chbm/toolbox-integration--should-forward-p)
             lsp-val)
    (setf (cdr lsp-val)
          (append (chbm/toolbox-integration--generate-cmd-prefix t)
                  (cdr lsp-val))))
    lsp-val)

(defun chbm/toolbox-integration--eglot-xref-adjust-location (xref-match)
  (when-let* ((_ (chbm/toolbox-integration--should-forward-p))
              (file-location (xref-match-item-location xref-match))
              (file-path (xref-file-location-file file-location))
              (_ (not (file-exists-p file-path))))
    (setf (xref-file-location-file file-location)
          (format "/%s:%s:%s"
                  (if (equal chbm/associated-container-engine "podman-remote")
                      "podman"
                    chbm/associated-container-engine)
                  chbm/associated-container
                  file-path)))
  xref-match)

(defun chbm/toolbox-integration--compile-add-prefix (&rest args)
  (when-let* ((_ (chbm/toolbox-integration--should-forward-p))
              (command (car args))
              (_ (not (string-match ".*? sh -c '\\(.*\\)'" (car command)))))
    (setf (car command)
          (concat (string-join (chbm/toolbox-integration--generate-cmd-prefix) " ")
                  (if (chbm/toolbox-integration--box-style-engine-p)
                      ;; toolbox takes care of using the correct directory, how nice!
                      (format " sh -c '%s'" (car command))
                    (format " sh -c 'cd %s && %s'" default-directory (car command))))))
  (car args))

(defun chbm/toolbox-integration--recompile-remove-prefix (&rest args)
  (when-let* ((_ (chbm/toolbox-integration--should-forward-p))
              (command (car compilation-arguments))
              (_ (string-match ".*? sh -c '\\(.*\\)'" command)))
    (setcar compilation-arguments (match-string 1 command)))
  (apply (car args) (cdr args)))

(defun eshell/toggle-container ()
  (if-let* ((_ (tramp-tramp-file-p default-directory))
            (tramp-file (tramp-dissect-file-name default-directory))
            (_ (equal (tramp-file-name-method tramp-file)
                      chbm/associated-container-engine)))
      (eshell/cd (tramp-file-name-localname tramp-file))
        (eshell/cd (format "/%s:%s:%s"
                     chbm/associated-container-engine
                     chbm/associated-container
                     default-directory))))

(defun chbm/toolbox-integration--run-command (mode-f)
  (if (chbm/toolbox-integration--should-forward-p)
      (let ((default-directory (format "/%s:%s:%s"
                                       chbm/associated-container-engine
                                       chbm/associated-container
                                       default-directory)))
        (call-interactively mode-f))
    (error "Toolbox integration not setup, call `chbm/register-container-for-integration'")))

(defun chbm/toolbox-integration-shell ()
  (interactive)
  (chbm/toolbox-integration--run-command #'shell))

(defun chbm/toolbox-integration-run-interactive-command ()
  (interactive)
  (chbm/toolbox-integration--run-command (intern-soft (read-extended-command))))

(defun chbm/toolbox-integration-async-shell-command ()
  (interactive)
  (chbm/toolbox-integration--run-command #'async-shell-command))

(defun chbm/toolbox-integration-shell-command ()
  (interactive)
  (chbm/toolbox-integration--run-command #'shell-command))

(defun chbm/toolbox-integration-eshell ()
  (interactive)
  (chbm/toolbox-integration--run-command #'eshell))

(defun chbm/toolbox-integration-register-container-for-integration ()
  "Register the current directory to use the tootlbox integration"
  (interactive)
  (add-dir-local-variable nil 'chbm/associated-container (read-string "Container name: "))
  (when-let* ((container-engine (read-string "Container engine: " "toolbox")))
    (add-dir-local-variable nil 'chbm/associated-container-engine container-engine)))

(define-minor-mode chbm/toolbox-integration-mode
  "A minor mode for forwarding some commands to a toolbox/distrobox
container, maybe more supported in the future."
  :lighter " Tlbx"
  :global t
  (let ((non-file-hooks-to-check
         '(compilation-mode-hook
           eshell-directory-change-hook
           eshell-mode-hook
           shell-mode-hook
           dired-mode-hook
           shell-command-mode-hook)))
    (if chbm/toolbox-integration-mode
        (progn
          (advice-add 'eglot--lookup-mode :filter-return #'chbm/toolbox-integration--set-to-container-lsp)
          (advice-add 'eglot--xref-make-match :filter-return #'chbm/toolbox-integration--eglot-xref-adjust-location)
          (advice-add 'compilation-start :filter-args #'chbm/toolbox-integration--compile-add-prefix)
          (advice-add 'recompile :around #'chbm/toolbox-integration--recompile-remove-prefix)

          (mapc (lambda (hook)
                  (add-hook hook #'hack-dir-local-variables-non-file-buffer))
                non-file-hooks-to-check))
      (advice-remove 'eglot--lookup-mode #'chbm/toolbox-integration--set-to-container-lsp)
      (advice-remove 'eglot--xref-make-match #'chbm/toolbox-integration--eglot-xref-adjust-location)
      (advice-remove 'compilation-start #'chbm/toolbox-integration--compile-add-prefix)
      (advice-remove 'recompile #'chbm/toolbox-integration--recompile-remove-prefix)

      (mapc (lambda (hook)
              (remove-hook hook #'hack-dir-local-variables-non-file-buffer))
            non-file-hooks-to-check))))

(defvar-keymap chbm/toolbox-integration-prefix
  "!" #'chbm/toolbox-integration-shell-command
  "&" #'chbm/toolbox-integration-async-shell-command
  "e" #'chbm/toolbox-integration-eshell
  "s" #'chbm/toolbox-integration-shell
  "o" #'chbm/toolbox-integration-run-interactive-command
  "r" #'chbm/toolbox-integration-register-container-for-integration)

(keymap-global-set "C-c t" chbm/toolbox-integration-prefix)

(provide 'chbm-toolbox-integration)
