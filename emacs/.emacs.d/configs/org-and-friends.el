(defconst chabam-ca-headers
  (apply 'concat
         `(;; Styling with simplecss
           "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />"
           ,(concat "<style>"
                    (with-temp-buffer
                      (insert-file-contents
                       (concat user-emacs-directory
                               "chabam-ca-assets/style.css"))
                      (buffer-string))
             "</style>")))
  "The headers used for my website")

(defun chbm/change-org-publish-location (dest)
  (interactive (list (setq dest (read-directory-name "New location: "))))
  (mapc (lambda (conf)
          (plist-put (cdr conf) ':publishing-directory (concat dest (car conf))))
        org-publish-project-alist))

(defun chbm/org-set-id-from-title ()
  "Sets a custom_id based on the title of the current heading"
  (interactive)
  (org-back-to-heading)
  (let* ((title (nth 4 (org-heading-components)))
         (custom-id (replace-regexp-in-string "-\\{2,\\}" "-" (string-replace " " "-" (downcase title)))))
    (org-set-property "CUSTOM_ID" custom-id)))

(defun chbm/set-website-config ()
  (let ((conf (locate-dominating-file default-directory "site-config.el")))
    (when conf
      (load-file (concat conf "site-config.el"))
      (message "org-publish site config loaded!"))))

(defun chbm/start-with-agenda ()
  (when (and (display-graphic-p)
             (not (string-prefix-p " *" (buffer-name))))
    (org-agenda nil "d")
    (delete-other-windows)))

(use-package org
  :ensure nil
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :hook ((org-mode . (lambda ()
                       (add-hook 'completion-at-point-functions #'cape-file nil t)))
         (org-mode . chbm/set-website-config))
  :config
  (require 'ox-publish)

  (setq org-directory "~/Documents/Org"
        org-agendas-directory (concat org-directory "/agendas/")

        org-icalendar-exclude-tags '("noexport")
        org-icalendar-combined-agenda-file (concat org-directory "/agendas/org.ics")
        org-icalendar-use-scheduled '(event-if-not-todo event-if-todo event-if-todo-not-done todo-start)
        org-icalendar-use-deadline '(event-if-not-todo event-if-todo event-if-todo-not-done todo-due)
        org-icalendar-timezone "America/Toronto"
        org-icalendar-date-time-format ";TZID=%Z:%Y%m%dT%H%M%S"

        icalendar-export-sexp-enumerate-all t
        icalendar-export-sexp-enumeration-days 365

        org-src-lang-modes `(("C" . c-ts)
                             ("C++" . c++-ts)
                             ("asymptote" . asy)
                             ("beamer" . latex)
                             ("calc" . fundamental)
                             ("cpp" . c++-ts)
                             ("ditaa" . artist)
                             ("desktop" . conf-desktop)
                             ("dot" . fundamental)
                             ("elisp" . emacs-lisp)
                             ("ocaml" . tuareg)
                             ("screen" . shell-script)
                             ("sqlite" . sql)
                             ("toml" . toml-ts)
                             ("shell" . sh)
                             ,@(org-src--get-known-shells))

        org-babel-results-keyword "results"

        org-export-with-date nil
        org-export-with-author nil
        org-export-time-stamp-file nil
        org-export-allow-bind-keywords t

        org-html-head chabam-ca-headers
        org-html-style-default nil
        org-html-htmlize-output-type 'css
        org-html-validation-link nil

        org-attach-use-inheritance t
        org-attach-auto-tag nil

        org-default-notes-file (expand-file-name (concat org-agendas-directory "a-classer.org") org-directory)
        org-agenda-files (directory-files-recursively org-agendas-directory  "\\.org$")
        org-archive-location (concat org-directory "/archive.org::datetree/")
        org-attach-id-dir (concat org-directory "/data/")
        org-todo-keywords '((sequence "FAIRE(f)" "COURS(c)" "ATTENTE(a)" "FAIT(F)")))

  (when chbm/emacs-containerized
    (setq org-file-apps '((auto-mode . emacs)
                          (directory . emacs)
                          ("\\.mm\\'" . chbm/xdg-open-host)
                          ("\\.x?html?\\'" . chbm/xdg-open-host)
                          ("\\.pdf\\'" . chbm/xdg-open-host))))
  ;; babel
  (org-babel-do-load-languages
   'org-babel-load-languages '((C . t)
                               (emacs-lisp . t)
                               (haskell . t)
                               (js . t)
                               (python . t)
                               (R . t)
                               (shell . t)))
  ;; calendar
  (add-to-list 'org-agenda-custom-commands
               '("W" "Weekend"
                 ((agenda ""))
                 ((org-agenda-overriding-header "Weekend")
                  (org-agenda-span 2)
                  (org-agenda-start-day "Saturday")
                  (org-agenda-show-all-dates t)
                  (org-agenda-include-empty-dates t)))
               t)

  (add-to-list 'org-agenda-custom-commands
               '("d" "Aujourd'hui - à faire"
                 ((agenda "" ((org-agenda-span 'day)))
                  (alltodo "")))
               t))

(use-package citar
  :ensure t
  :after org
  :hook (org-mode . (lambda ()
                      (setq org-cite-insert-processor 'citar
                            org-cite-follow-processor 'citar
                            org-cite-activate-processor 'citar))))

(use-package htmlize
  :ensure t
  :commands (org-export-dispatch))

(use-package org-download
  :ensure t
  :hook ((org-mode . (lambda ()
                       (require 'org-download)))
         (dired-mode . org-download-enable))
  :config
  (setq-default org-download-image-dir "./images"
                org-download-heading-lvl nil))

(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook #'auto-fill-mode)

;; (use-package org-caldav
;;   :config
;;   (setq org-caldav-url "http://caldav.minus/dav.php/calendars/chabam")
;;   (setq org-caldav-calendar-id "default")
;;   (setq org-caldav-inbox (concat org-agendas-directory "caldav.org"))
;;   (setq org-caldav-files nil))

;; TODO: convert to a proper function
(defalias 'org-emphasize-accronym
  (kmacro "C-SPC C-f C-c C-x C-f * C-d C-x 8 <return> z e r o <return> M-f C-f"))

(provide 'org-and-friends)
