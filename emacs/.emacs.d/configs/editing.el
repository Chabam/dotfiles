;; Increase undo values, I make a lot of mistakes
(setq undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000))

(delete-selection-mode 1)

;; Remove blinking for delete-pair
(setq delete-pair-blink-delay 0)

