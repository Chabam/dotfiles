(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruvbox-dark-medium))
 '(custom-safe-themes
   '("98ef36d4487bf5e816f89b1b1240d45755ec382c7029302f36ca6626faf44bbd" "046a2b81d13afddae309930ef85d458c4f5d278a69448e5a5261a5c78598e012" "1594eb8fc081be254c7df7b2a37e9808f79c94863366da6d34bbe735519a30f5" "b0cbcb2fa0c69ab36f4505fec9967969b73327f1b8564f9afface8afd216bc93" "7b8f5bbdc7c316ee62f271acf6bcd0e0b8a272fdffe908f8c920b0ba34871d98" "8d146df8bd640320d5ca94d2913392bc6f763d5bc2bb47bed8e14975017eea91" default))
 '(font-use-system-font t)
 '(package-selected-packages
   '(transpose-frame multiple-cursors modus-themes auctex gruvbox-theme which-key corfu racket-mode org lsp-mode yasnippet lsp-treemacs projectile hydra flycheck avy dap-mode lsp-scheme))
 '(tool-bar-mode nil))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(set-face-attribute 'default nil :height 110 :family "Iosevka")

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq dired-listing-switches "-aBhlv  --group-directories-first")

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; Org
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(use-package org
  :config
  (setopt org-startup-indented t
          org-hide-emphasis-markers t
          org-pretty-entities t
          org-src-fontify-natively t
          org-fontify-whole-heading-line t
          org-fontify-quote-and-verse-blocks t
          org-edit-src-content-indentation 2
          org-hide-block-startup nil
          org-src-tab-acts-natively t
          org-src-preserve-indentation nil
          org-startup-folded t
          org-cycle-separator-lines 2
          org-hide-leading-stars t
          org-highlight-latex-and-related '(native)
          org-goto-auto-isearch nil
          )
)

(require 'org-tempo)

;; lsp

;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
;; (helm-mode)
;; (require 'helm-xref)
;; (define-key global-map [remap find-file] #'helm-find-files)
;; (define-key global-map [remap execute-extended-command] #'helm-M-x)
;; (define-key global-map [remap switch-to-buffer] #'helm-mini)

(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1
      lsp-clangd-binary-path "/usr/bin/clangd"
      )  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number ((t (:foreground "#7c6f64" :background "#3c3836" :inherit default))))
 '(line-number-current-line ((t (:foreground "#fe8019" :background "#504945" :inherit default)))))

;; AUCTeX
(use-package tex
  :ensure auctex)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
