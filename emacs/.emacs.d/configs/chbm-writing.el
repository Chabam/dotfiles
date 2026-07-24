(add-hook 'text-mode-hook #'auto-fill-mode)

(use-package mu4e
  :ensure nil
  :commands (mu4e)
  :hook ((mu4e-compose-mode . (lambda ()
                                (display-line-numbers-mode)
                                (set-fill-column 72)))
         (mu4e-thread-mode . mu4e-thread-fold-all))
  :bind (("C-c m" . mu4e-transient-menu))
  :config
  (setq mu4e-contexts
        (list (make-mu4e-context
               :name "Gmail"
               :vars '((user-mail-address . "fchabot1337@gmail.com")
                       (user-full-name . "Félix Chabot")
                       (smtpmail-smtp-server . "smtp.gmail.com")
                       (smtpmail-stream-type . starttls)
                       (smtpmail-smtp-service . 587)
                       (mu4e-drafts-folder . "/gmail/[Gmail]/Drafts")
                       (mu4e-sent-folder . "/gmail/[Gmail]/Sent Mail")
                       (mu4e-refile-folder . "/gmail/[Gmail]/All Mail")
                       (mu4e-trash-folder . "/gmail/[Gmail]/Trash")
                       (mu4e-maildir-shortcuts . ((:name "Inbox"
                                                   :maildir "/gmail/[Gmail]/All Mail"
                                                   :key ?i)
                                                  (:name "Sent"
                                                   :maildir "/gmail/[Gmail]/Sent Mail"
                                                   :key ?s)))))

              (make-mu4e-context
               :name "UdeS"
               :vars '((user-mail-address . "chaf2717@usherbrooke.ca")
                       (user-full-name . "Félix Chabot")
                       (smtpmail-smtp-server . "smtp.office365.com")
                       (smtpmail-stream-type . starttls)
                       (smtpmail-smtp-service . 587)
                       (mu4e-drafts-folder . "/udes/Brouillons")
                       (mu4e-sent-folder . "/udes/Éléments envoyés")
                       (mu4e-refile-folder . "/udes/Inbox")
                       (mu4e-trash-folder . "/udes/Éléments supprimés")
                       (mu4e-maildir-shortcuts . ((:name "Inbox"
                                                   :maildir "/udes/Inbox"
                                                   :key ?i)
                                                  (:name "Sent"
                                                   :maildir "/udes/Éléments envoyés"
                                                   :key ?s)))))))
  (setq mu4e-compose-context-policy 'ask-if-none)
  (setq mu4e-maildir "~/.mail")
  (setq mu4e-completing-read-function 'completing-read)
  (setq mu4e-index-lazy-check t)
  (setq mu4e-update-interval 180)
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-attachment-dir "~/Downloads")

  (setq mu4e-headers-fields
        '((:human-date . 12)
          (:from-or-to . 22)
          (:subject)
          (:flags)))

  (setq mu4e-use-fancy-chars t)
  (setq mu4e-headers-attach-mark    '("a" . "📎")
        mu4e-headers-calendar-mark  '("c" . "📅")
        mu4e-headers-draft-mark     '("D" . "✏")
        mu4e-headers-encrypted-mark '("x" . "🔒")
        mu4e-headers-flagged-mark   '("F" . "🚩")
        mu4e-headers-list-mark      '("l" . "🔈")
        mu4e-headers-new-mark       '("N" . "✨")
        mu4e-headers-passed-mark    '("P" . "↪")
        ;; Disabling cause, that's basically all my mail
        mu4e-headers-personal-mark  '(""  .   "")
        mu4e-headers-replied-mark   '("R" . "↩")
        mu4e-headers-seen-mark      '("S" . "👀")
        mu4e-headers-signed-mark    '("s" . "🔑")
        mu4e-headers-trashed-mark   '("T" . "🗑️")
        mu4e-headers-unread-mark    '("u" . "📩"))

  (setq mu4e-get-mail-command "mbsync -a")

  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program (executable-find "msmtp"))
  (setq message-sendmail-f-is-evil t)
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  (setq message-sendmail-envelope-from 'header)
  (setq message-signature "Félix Chabot")

  (with-eval-after-load "mm-decode"
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext")
    (add-to-list 'mm-discouraged-alternatives "multipart/related"))

  (setq mu4e-change-filenames-when-moving t)
  (add-hook 'dired-mode-hook #'turn-on-gnus-dired-mode))

(use-package jinx
  :hook ((org-mode . jinx-mode)
         (text-mode . jinx-mode)
         (markdown-mode . jinx-mode)
         (log-edit-mode . jinx-mode))
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :ensure t
  :config
  (setq jinx-languages "en_CA fr_CA"))


(provide 'chbm-writing)
