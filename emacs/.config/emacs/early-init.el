(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

(setq process-adaptive-read-buffering nil)

(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024))
            (setq gc-cons-percentage 0.1)))
