;;;
;;; command.scm
;;;
;;; Evaluate new code from the user. The procedure read-command
;;; creates a text entry zone, sort of like the emacs minibuffer,
;;; where the user can input s-expressions to be evaluated.
;;;

(define cmd-history '())

(define cmd-box
  (sge-entity-set-rgba
   (sge-entity-set-animation
    (sge-entity-set-zorder
     (sge-entity-add-attrib
      (sge-entity-add-attrib
       (sge-entity-create) sge-attrib-hidden)
      sge-attrib-position-absolute) 1000) anim-pixel)
   0 0 0 100))

(define cmd-box-height 25)

(define (cmd-mode)
  (define view-size (sge-camera-get-view-size))
  (sge-entity-remove-attrib cmd-box sge-attrib-hidden)
  (sge-entity-set-scale cmd-box (car view-size) cmd-box-height)
  (sge-entity-set-position cmd-box 0 (- (cdr view-size) cmd-box-height))
  (sleep 5)
  (sge-entity-add-attrib cmd-box sge-attrib-hidden)
  ;; If we don't reset the game's delta timers to ignore
  ;; elapsed time during text entry, the game will think that
  ;; a huge amount of time elapsed when in reality the game
  ;; was merely paused.
  (sge-timer-reset *delta-timer*)
  (sge-timer-reset *logic-timer*))
