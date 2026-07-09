;; -*- lexical-binding: t -*-

(let ((default-gc-thresh gc-cons-threshold)
      (default-gc-percent gc-cons-percentage))
  (setq gc-cons-threshold most-positive-fixnum)
  (setq gc-cons-percentage 1.0)

  (add-hook 'emacs-startup-hook
            (lambda ()
              (set-buffer "*scratch*")
              (insert (format ";; Emacs ready in %s with %d garbage collections.\n\n"
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done))))

  (run-with-idle-timer
   5 nil
   (lambda ()
     (setq gc-cons-threshold default-gc-thresh)
     (setq gc-cons-percentage default-gc-percent))))
