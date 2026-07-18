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

(use-package ox
  :ensure nil
  :commands (org-export-dispatch org-export-to-file)
  :config

  (setq org-export-with-date nil)
  (setq org-export-with-author nil)
  (setq org-export-time-stamp-file nil)
  (setq org-export-allow-bind-keywords t)
  (setq org-export-with-broken-links t))

(use-package ox-icalendar
  :ensure nil
  :after ox
  :commands (org-icalendar-export-to-ics)
  :config
  (setq org-icalendar-exclude-tags '("noexport"))
  (setq org-icalendar-combined-agenda-file "org.ics")
  (setq org-icalendar-use-scheduled '(event-if-not-todo event-if-todo event-if-todo-not-done todo-start))
  (setq org-icalendar-use-deadline '(event-if-not-todo event-if-todo event-if-todo-not-done todo-due))
  (setq org-icalendar-timezone "America/Toronto")
  (setq org-icalendar-date-time-format ";TZID=%Z:%Y%m%dT%H%M%S")

  (setq icalendar-export-sexp-enumerate-all t)
  (setq icalendar-export-sexp-enumeration-days 365))

(use-package ox-html
  :ensure nil
  :commands (org-html-export-to-html)
  :after ox
  :config

  (setq org-html-head chabam-ca-headers)
  (setq org-html-style-default nil)
  (setq org-html-htmlize-output-type 'css)
  (setq org-html-validation-link nil))

(use-package org-agenda
  :ensure nil
  :bind (("C-c a" . org-agenda))
  :config
  (setq org-agenda-files '("calendrier.org" "taches.org" "inbox.org"))
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

(use-package org
  :ensure nil
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture))
  :hook ((org-mode . (lambda ()
                       (add-hook 'completion-at-point-functions #'cape-file nil t)))
         (org-mode . chbm/set-website-config))
  :config

  (setq org-directory "~/Documents/Org")
  (setq org-src-lang-modes `(("C" . c-ts)
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
                             ,@(org-src--get-known-shells)))

  (setq org-babel-results-keyword "results")

  (setq org-attach-use-inheritance t)

  (setq org-default-notes-file (file-name-concat org-directory "inbox.org"))
  (setq org-capture-templates '(("t" "Tâche" entry (file "")
		                         "* FAIRE %?\n%u\n%a")))

  (setq org-archive-location "archive.org::datetree/")
  (setq org-attach-id-dir "data")
  (setq org-todo-keywords '((sequence "FAIRE(f)" "COURS(c)" "ATTENTE(a)" "FAIT(F)")))

  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-targets `((,(remove "inbox.org" org-agenda-files) :maxlevel . 3)))

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
                               (shell . t))))

;; (use-package citar
;;   :ensure t
;;   :after org
;;   :hook (org-mode . (lambda ()
;;                       (setq org-cite-insert-processor 'citar
;;                             org-cite-follow-processor 'citar
;;                             org-cite-activate-processor 'citar))))

;; (use-package htmlize
;;   :ensure t
;;   :commands (org-export-dispatch))

(use-package org-download
  :ensure t
  :hook ((org-mode . (lambda ()
                       (require 'org-download))))
  :commands (org-download-clipboard)
  :config
  (setq-default org-download-image-dir "./images"
                org-download-heading-lvl nil)
  (add-hook 'dired-mode-hook #'org-download-enable))

(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook #'auto-fill-mode)

(use-package org-caldav
  :ensure t
  :commands (org-caldav-sync)
  :config
  (setq org-caldav-url "https://nextcloud.chabam.ca/remote.php/dav/calendars/")
  (setq org-caldav-calendar-id "chabam/main")
  (setq org-caldav-sync-todo t)
  (setq org-icalendar-include-todo 'all)
  (setq org-caldav-inbox (file-name-concat org-directory "inbox.org"))
  (setq org-caldav-files (mapcar (lambda (f) (file-name-concat org-directory f)) org-agenda-files))
  (setq org-caldav-save-directory (file-name-concat org-directory ".org-caldav"))
  (setq org-caldav-todo-percent-states
        '((0 "FAIRE") (25 "COURS") (50 "ATTENTE") (100 "FAIT"))))

;; TODO: convert to a proper function
(defalias 'org-emphasize-accronym
  (kmacro "C-SPC C-f C-c C-x C-f * C-d C-x 8 <return> z e r o <return> M-f C-f"))

(provide 'org-and-friends)
