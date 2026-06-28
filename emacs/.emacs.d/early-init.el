(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold (* 32 1024 1024))
   (setq gc-cons-percentage 0.1)))
