;; Thanks Prot!
(defun prot-spell-ispell-display-buffer (buffer)
  "Function to override `ispell-display-buffer' for BUFFER.
Use this as `advice-add' to override the aforementioned Ispell
function.  Then you can control the buffer's specifics via
`display-buffer-alist' (how it ought to be!)."
  (pop-to-buffer buffer)
  (set-window-point (get-buffer-window buffer) (point-min)))

(advice-add #'ispell-display-buffer :override #'prot-spell-ispell-display-buffer)

(use-package mu4e
  :ensure nil
  :commands (mu4e)

  :config
  (setq mu4e-maildir "~/.mail")
  (setq mu4e-completing-read-function 'completing-read)

  (add-to-list 'mu4e-contexts
               (make-mu4e-context
                :name "Gmail"
                :vars '((user-mail-address . "fchabot1337@gmail.com")
                        (user-full-name . "Félix Chabot")
                        (smtpmail-smtp-server . "smtp.gmail.com")
                        (smtpmail-stream-type . starttls)
                        (smtpmail-smtp-service . 587)
                        (mu4e-drafts-folder . "/gmail/[Gmail]/Drafts")
                        (mu4e-sent-folder . "/gmail/[Gmail]/Sent Mail")
                        (mu4e-refile-folder . "/gmail/[Gmail]/All Mail")
                        (mu4e-trash-folder . "/gmail/[Gmail]/Trash"))))

  (add-to-list 'mu4e-contexts
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
                        (mu4e-trash-folder . "/udes/Éléments supprimés"))))

  (setq mail-user-agent 'mu4e-user-agent)

  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-update-interval 300)

  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program (executable-find "msmtp"))
  (setq message-sendmail-f-is-evil t)
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  (setq message-sendmail-envelope-from 'header)

  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-compose-format-flowed t)
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses t))

(provide 'writing)
