;; -*- lexical-binding: t -*-

(use-package auctex
  :ensure t
  :hook (LaTeX-mode . turn-on-reftex)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default Tex-master nil)
  (setq reftex-plug-into-AUCTeX t)
  (when chbm/emacs-containerized
    (setq TeX-view-program-list '(("xdg-open" "flatpak-spawn --host xdg-open %o"))))
  (setq TeX-view-program-selection '((output-pdf "xdg-open"))))

(use-package haskell-mode :ensure t)

(use-package pascal
  :ensure nil
  :config
  (setq pascal-font-lock-keywords '(("\\_<\\(function\\|pro\\(cedure\\|gram\\)\\)[ 	]+\\([[:alpha:]][[:alnum:]_]*\\)" (1 font-lock-keyword-face) (3 font-lock-function-name-face))
                                    ("\\_<\\(array\\|boolean\\|c\\(har\\|onst\\)\\|file\\|integer\\|re\\(al\\|cord\\)\\|type\\|var\\)\\_>" . font-lock-type-face)
                                    ("\\_<\\(label\\|external\\|forward\\)\\_>" . font-lock-constant-face)
                                    ("\\_<\\([0-9]+\\)[ 	]*:" 1 font-lock-function-name-face)
                                    "\\_<\\(and\\|begin\\|case\\|do\\|e\\(lse\\|nd\\)\\|for\\|i[fn]\\|not\\|o[fr]\\|repeat\\|t\\(hen\\|o\\)\\|until\\|w\\(hile\\|ith\\)\\)\\_>"
                                    ("\\_<\\goto\\_>" . font-lock-keyword-face))))

(defun chbm/is-first-sibling (&optional node-t parent-node-t)
  (lambda (node parent &rest _)
    (and node
         parent
         (or (not node-t)
             (string-match-p
              node-t (treesit-node-type node)))
         (or (not parent-node-t)
             (string-match-p
              parent-node-t (treesit-node-type parent)))
         (not (treesit-node-prev-sibling node t)))))

(defun chbm/is-last-sibling (&optional node-t parent-node-t)
  (lambda (node parent &rest _)
    (and node
         parent
         (or (not node-t)
             (string-match-p
              node-t (treesit-node-type node)))
         (or (not parent-node-t)
             (string-match-p
              parent-node-t (treesit-node-type parent)))
         (not (treesit-node-next-sibling node t)))))

(defun chbm/bsd-style-indent ()
  "Override the built-in BSD indentation style with some additional rules"
  `(
    ((n-p-gp nil "declaration_list" "namespace_definition") parent-bol 0)
    ((match "compound_statement" "for_range_loop") standalone-parent 0)
    ((match "compound_statement" "try_statement") standalone-parent 0)
    ((match "compound_statement" "catch_clause") standalone-parent 0)
    ((node-is "catch_clause") parent-bol 0)
    ((node-is "access_specifier") parent-bol 2)
    ((match "field_declaration_list" "struct_specifier") parent-bol 0)
    ((match "compound_statement" "lambda_expression") parent-bol c-ts-mode-indent-offset)
    ((node-is "field_initializer_list") parent-bol c-ts-mode-indent-offset)
    ((parent-is "field_initializer_list") grand-parent c-ts-mode-indent-offset)
    ((chbm/is-first-sibling "parameter_declaration" "parameter_list") standalone-parent c-ts-mode-indent-offset)
    ((chbm/is-first-sibling nil "argument_list") standalone-parent c-ts-mode-indent-offset)
    ((chbm/is-last-sibling ")" "parameter_list") standalone-parent 0)
    ((chbm/is-last-sibling ")" "argument_list") standalone-parent 0)
    ((parent-is "binary_expression") prev-sibling c-ts-mode-indent-offset)
    ((parent-is "argument_list") prev-sibling 0)
    ((parent-is "parameter_list") prev-sibling 0)
    ,@(alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))

(defun chbm/set-c-style-indent ()
  "Sets up indentation with my treesit indent style"
  (setq-local c-ts-mode-indent-offset 4)
  (c-ts-mode-set-style 'chbm/bsd-style-indent))

(defun chbm/ff-find-other-file ()
  (interactive)
  (ff-find-other-file nil t))

(defun chbm/set-cc-search-dirs-project ()
  "Adds likely locations to look for other files (headers) based on the project"
  (interactive)
  (when (project-current)
    (let* ((project-root (caddr (project-current)))
           (is-not-hidden-p (lambda (f) (null (string-match "^\\." f))))
           (is-not-build-dir-p (lambda (f) (null (string-match "build" f))))
           (dir-recurse-p (lambda (p)
                            (let ((f (file-name-nondirectory p)))
                              (and (funcall is-not-build-dir-p f)
                                   (funcall is-not-hidden-p f)))))
           (exclude-p (lambda (f)
                        (and (file-directory-p f)
                             (funcall is-not-hidden-p f))))
           (get-subdirs (lambda (dir)
                          (when (file-directory-p dir)
                            (seq-filter exclude-p
                                        (directory-files-recursively dir "" t dir-recurse-p)))))
           (sub-dirs (funcall get-subdirs project-root)))
      (setq-local cc-search-directories
                  (append
                   (list
                    "."
                    "/usr/include"
                    "/usr/local/include/*")
                   (mapcar (lambda (dir) (file-name-concat dir "*")) sub-dirs))))))

(use-package c++-ts-mode
  :ensure nil
  :hook ((c++-ts-mode . chbm/set-c-style-indent)
         (ff-pre-find . chbm/set-cc-search-dirs-project)))

(use-package c-ts-mode
  :ensure nil
  :hook ((c-ts-mode . (lambda ()
                        (setq c-ts-mode-indent-offset 4)
                        (c-ts-mode-set-style 'bsd)))
         (ff-pre-find . chbm/set-cc-search-dirs-project)))

(use-package xml-mode
  :ensure nil
  :mode "\\.cts")

(use-package ess
  :ensure t
  :hook ((ess-r-mode . (lambda ()
                         (setq-local ess-indent-offset 2
                                     comment-column 0
                                     standard-indent 2
                                     tab-width 2
                                     indent-tabs-mode nil)
                                        ; Don't add random R package
                                        ; to the project list
                                        ; please...
                         (add-hook 'project-find-functions #'project-try-vc nil 'local)))
         (ess-r-package-enter . (lambda ()
                                        ; Don't add random R package
                                        ; to the project list
                                        ; please...
                                  (add-hook 'project-find-functions #'project-try-vc nil 'local))))
  :config
  (setq ess-use-ido nil))

(use-package cmake-mode :ensure t)

(use-package clang-format :ensure t)

(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.yml")

(use-package dockerfile-ts-mode
  :ensure nil
  :mode "Containerfile")

(use-package systemd
  :ensure t
  :mode (("\\.container\\'" . systemd-mode)
         ("\\.pod\\'" . systemd-mode)))

(use-package markdown-mode :ensure t)

(use-package make-mode
  :ensure t
  :hook (makefile-gmake-mode . whitespace-mode))

(use-package zig-mode
  :ensure t
  :config
  (setq zig-format-on-save nil))

(use-package racket-mode
  :ensure t
  :mode "\\.rkt\\'")

(use-package glsl-mode
  :ensure t
  :mode (("\\.vs\\'" . glsl-mode)
         ("\\.fs\\'" . glsl-mode)
         ("\\.cs\\'" . glsl-mode))
  :hook ((glsl-mode . (lambda () (c-set-style "k&r")))))

(use-package conf-mode
  :ensure nil
  :mode (("\\.desktop\\'" . conf-desktop-mode)))

(provide 'programming-languages)
