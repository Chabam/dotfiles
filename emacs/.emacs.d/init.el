(load-file "~/.emacs.d/chabam-light-theme.el")
(load-file "~/.emacs.d/chabam-dark-theme.el")

(defun chbm-set-fonts ()
  "Set fonts for frame and after theme"
  (when (display-graphic-p)
    (set-face-attribute 'default nil :family "Iosevka" :height 120)
    (set-face-attribute 'fixed-pitch nil :family "Iosevka")
    (set-face-attribute 'variable-pitch nil :family "Iosevka")
    (set-face-attribute 'tab-bar nil :height 120)
    (set-face-attribute 'tab-bar-tab nil :height 120)
    (set-face-attribute 'tab-bar-tab-inactive nil :height 120)))

(defun chbm-start-with-agenda ()
  (when (and (display-graphic-p)
             (not (string-prefix-p " *" (buffer-name))))
    (org-agenda-list)
    (delete-other-windows)))

(use-package emacs
  :bind (("C-." . duplicate-line)
         ("C-x C-b" . ibuffer))
  :hook (((prog-mode text-mode) . (lambda () (setq show-trailing-whitespace t)
                                    (column-number-mode)
                                    (display-line-numbers-mode)))
         ((org-mode text-mode) . auto-fill-mode))
  :ensure nil
  :custom
  (custom-file "~/.emacs.d/custom.el")
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; Remove the bell
  (ring-bell-function 'ignore)
  ;; Remove splash screen
  (inhibit-startup-screen t)

  (text-mode-ispell-word-completion nil)

  (completion-styles '(basic substring partial-completion flex))
  (vc-follow-symlinks t)
  (read-extended-command-predicate #'command-completion-default-include-p)

  :init
  (setq use-package-always-ensure t)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)

  ;; Putting files somewhere else
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))

  ;; Smooth scroll
  (pixel-scroll-precision-mode)
  (pixel-scroll-mode)

  ;; Hiding ui elements
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0)

  (winner-mode)
  (electric-pair-mode)

  (setq indent-line-function 'insert-tab
        tab-always-indent 'complete
        tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-show 1
        set-mark-command-repeat-pop t
        deleted-by-moving-to-trash t
        show-paren-when-point-in-periphery t
        show-paren-when-point-inside-paren t
        show-paren-context-when-offscreen t
        vc-handled-backends '(Git)
        default-frame-alist '((font . "Iosevka-12")
                              (width . 100)
                              (height . 40)
                              (vertical-scroll-bars . nil))
		whitespace-style '(face indentation tabs tab-mark spaces space-mark
                                newline newline-mark trailing))
  (setq-default standard-indent 4
                tab-width 4
                indent-tabs-mode nil)
  (which-key-mode)
  (recentf-mode)
  (require 'ansi-color)
  (advice-add 'save-buffers-kill-terminal :before-while (lambda (_) (yes-or-no-p "Really quit Emacs? ")))

  ;; Trying to properly set fonts
  (chbm-set-fonts)
  (advice-add 'load-theme :after #'chbm-set-fonts)
  (add-hook 'server-after-make-frame-hook
            (lambda ()
              (chbm-set-fonts)
              (chbm-start-with-agenda))))

(use-package transpose-frame)

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package project
  :ensure nil
  :init
  (setq project-switch-commands '((project-find-file "Find file") (project-find-regexp "Find regexp")
                                  (project-find-dir "Find directory") (magit-project-status "Magit" "m")
                                  (project-any-command "Other"))))

(use-package no-littering
  :init
  (require 'no-littering)
  (no-littering-theme-backups)
  (let ((dir (no-littering-expand-var-file-name "lock-files/")))
    (make-directory dir t)
    (setq lock-file-name-transforms `((".*" ,dir t)))))

(use-package vertico
  :custom
  (vertico-count 10)
  :init
  (vertico-mode))

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package compile
  :ensure nil
  :init
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  (setq compilation-skip-threshold 2))

(use-package flymake
  :ensure nil
  :config (setq flymake-indicator-type 'margins))

(use-package corfu
  :custom
  ;; Autocomplete settings
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.2)
  (corfu-quit-no-match 'separator)
  (corfu-quit-at-boundary t)
  (corfu-preview-current nil)

  (corfu-cycle t)
  (corfu-popupinfo-delay 0.5)
  (corfu-left-margin-width 0)
  (corfu-right-margin-width 0)
  :bind (:map corfu-map
              ("RET" . nil))
  :hook (inferior-python-mode . (lambda () (corfu-auto nil)))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package org-mode
  :ensure nil
  :mode "\\.org\\'"
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :init
  (require 'org-tempo)
  (setq org-directory "~/Notes"
        org-agendas-directory (concat org-directory "/Agendas/")
        org-default-notes-file (expand-file-name (concat org-agendas-directory "Misc.org") org-directory)
        org-agenda-files (directory-files-recursively org-agendas-directory  "\\.org$")
        org-archive-location (concat org-directory "/archive.org::datetree/")
        org-attach-id-dir (concat org-directory "/Data/")
        org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w)" "DONE(d)"))))

(use-package org-roam
  :bind (("C-c C-r c" . org-roam-capture)
         ("C-c C-r f" . org-roam-node-find))
  :config
  (setq org-roam-directory (concat org-directory "/Roam/"))
  (org-roam-db-autosync-mode))

(use-package org-alert
  :config
  (org-alert-enable)
  (setq alert-default-style 'libnotify
        org-alert-time-match-string "\\(?:SCHEDULED\\|DEADLINE\\):.*?<.*?\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)\\(-[0-9]\\{2\\}:[0-9]\\{2\\}\\)?.*>"
        org-alert-notify-after-event-cutoff 10))

(use-package orderless
  :custom
  (completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (completion-matching-styles '(orderless-regexp)))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :hook (dired-mode . (lambda () (dired-hide-details-mode 1)))
  :custom
  (dired-listing-switches "-aBhlv  --group-directories-first")
  (dired-dwim-target t))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  (setq-default treesit-font-lock-level 4))

(defun chbm-indent-style()
    "Override the built-in BSD indentation style with some additional rules"
    `(
      ((node-is ")") parent-bol 0)
      ((node-is "(") parent-bol 0)
      ((node-is "}") parent-bol 0)
      ((node-is "{") parent-bol 0)
      ((n-p-gp nil nil "namespace_definition") grand-parent 0)
      ((node-is "preproc") column-0 0)
      ((node-is "access_specifier") parent-bol 2)
      ((node-is "field_initializer_list") parent-bol 4)
      ((node-is "field_initializer") (nth-sibling 1) 0)
      ((parent-is "compound_statement") parent-bol c-ts-mode-indent-offset)
      ((node-is "compound_statement") parent-bol 0)
      ((parent-is "parameter_list") parent-bol 4)
      ((parent-is "argument_list") parent-bol 4)
      ((parent-is "initializer_list") grand-parent c-ts-mode-indent-offset)

      ,@(alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))

(use-package c-ts-mode
  :ensure nil
  :bind ("C-c o" . ff-find-other-file)
  :init
  (setq-default c-ts-mode-indent-offset 4)
  (setq-default c-ts-mode-indent-style #'chbm-indent-style))

(use-package magit)

(use-package diff-hl
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

(use-package eglot
  :ensure nil
  :hook ((c++-ts-mode python-ts-mode cmake-ts-mode racket-mode haskell-mode)
         . eglot-ensure)
  :bind (("C-x C-a" . eglot-code-actions)
         ("C-x C-r" . eglot-rename))
  :config
  (setq eglot-autoshutdown t)
  (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider :documentOnTypeFormattingProvider)
  (add-to-list 'eglot-server-programs
               '((org-mode (LaTeX-mode :language-id "latex")) . ("ltex-ls-plus" "--server-type" "TcpSocket" "--port" :autoport)))
  (setq-default eglot-workspace-configuration
                '((:ltex . (:language "auto"
                            :completionEnabled t
                            :latex (:environments (:lstlisting "ignore" :circuitikz "ignore" )
                                    :commands (:\\lstset{} "ignore"
                                               :\\lstdefinelanguage{}{} "ignore"
                                               :\\lstinputlisting{} "ignore"))
                            :disabledRules (:fr ["FRENCH_WHITESPACE"]))))))

(use-package direnv
  :init (direnv-mode))

(use-package eglot-inactive-regions
  :custom
  (eglot-inactive-regions-style 'darken-foreground)
  (eglot-inactive-regions-opacity 0.4)
  :config
  (eglot-inactive-regions-mode 1))

(use-package wgrep)

(use-package expand-region
  :bind ("C-=" . 'er/expand-region))

(use-package vundo)

(use-package embrace
  :hook ((org-mode . embrace-org-mode-hook)
         (LaTeX-mode . embrace-LaTeX-mode-hook))
  :bind ("C-c s" . embrace-commander))

(use-package vterm)

(defun set-auto-dark (f)
  (when (display-graphic-p f)
    (with-selected-frame f
      (auto-dark-mode t))))

(use-package auto-dark
  :init (if (daemonp)
            (add-hook 'after-make-frame-functions #'set-auto-dark)
            (auto-dark-mode t))
  :hook ((auto-dark-dark-mode . chbm-set-fonts)
         (auto-dark-light-mode . chbm-set-fonts))
  :custom (auto-dark-themes '((chabam-dark) (chabam-light))))

;; Various modes
(use-package racket-mode
  :mode "\\.rkt\\'")

(use-package auctex
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default Tex-master nil))

(use-package haskell-mode
  :mode "\\.hs\\'"
  :hook (haskell-mode . interactive-haskell-mode))

(use-package tramp
  :ensure nil
  :config
  (setq remote-file-name-inhibit-locks t
        tramp-use-scp-direct-remote-copying t
        remote-file-name-inhibit-auto-save-visited t
        tramp-copy-size-limit (* 1024 1024) ;; 1MB
        tramp-verbose 2)

  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)

  (setq magit-tramp-pipe-stty-settings 'pty)
  (with-eval-after-load 'tramp
    (with-eval-after-load 'compile
      (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options))))

(use-package gud
  :ensure nil
  :init
  (setq gdb-debuginfod-enable-setting nil))
