(define *resource-path* (sge-resource-path))
(define *key-vec* (make-vector sge-key-count #f))
(define *delta-timer* (sge-timer-create))
(define *logic-timer* (sge-timer-create))
(define *low-power-mode* #t)

(define (require fname)
  (load (string-append *resource-path* "scripts/" fname)))

(require "utils.scm")
(require "anim-manager.scm")
(require "controls.scm")
(require "class.scm")
(require "player.scm")
(require "math.scm")
(require "level.scm")
(require "level-manager.scm")
(require "command.scm")

(define (lpm-sleep)
  (let ((logic-usec (sge-timer-reset *logic-timer*)))
    (sge-microsleep (max 0 (- 2000 logic-usec)))))

(define (logic-loop)
  (cond
   ((not (sge-is-running?))
    '())
   (else
    (logic-step (sge-timer-reset *delta-timer*))
    (poll-events)
    (cond (*low-power-mode* (lpm-sleep)))
    (logic-loop))))

(define (logic-step dt)
  (*lvl-current* 'update dt))

(define (poll-events)
  (let ((event (sge-poll-event)))
    (cond
     ((null? event) '())
     (else
      (let ((event-code (vector-ref event 0)))
        (cond
         ((eq? event-code sge-event-key-pressed)
          (let ((pressed (vector-ref event 1)))
            (vector-set! *key-vec* pressed #t)
            (cond ((eq? pressed sge-key-esc)
                   (cmd-mode)
                   ;; If we don't reset the game's delta timers to ignore
                   ;; elapsed time during the repl, the game will think that
                   ;; a huge amount of time elapsed when in reality it
                   ;; was merely paused.
                   (sge-timer-reset *delta-timer*)
                   (sge-timer-reset *logic-timer*)))))
         ((eq? event-code sge-event-key-released)
          (let ((released (vector-ref event 1)))
            (vector-set! *key-vec* released #f)))))
      (poll-events)))))

(lvl-switch 'apt-0)

(cmd-load-history)
(logic-loop)
(cmd-save-history)
