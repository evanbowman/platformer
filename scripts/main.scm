;;;
;;; main.scm
;;;
;;; Entry point for the game, the engine starts executing
;;; scripts by looking up and calling the symbol 'main'.
;;;

(include "class.scm")
(include "math.scm")
(include "player.scm")
(include "level.scm")
(include "wall.scm")
(include "controls.scm")
(include "animations.scm")
(include "command.scm")
(include "utility.scm")

(define (main)
  (cmd-load-history)  
  (logic-loop)
  (cmd-save-history))

(define *delta-timer* (sge-timer-create))
(define *logic-timer* (sge-timer-create))
(define *low-power-mode* #t)

(sge-record-events #t)

(define *key-vec* (make-vector sge-key-count #f))

(define (poll-events)
  (let ((event (sge-poll-events)))
    (cond ((null? event) '())
          (else
           (case (vector-ref event 0)
             ((sge-event-key-pressed)
              (let ((pressed (vector-ref event 1)))
                (cond
                 ((eq? pressed sge-key-esc)
                  (cmd-mode))
                 (else
                  (vector-set! *key-vec* pressed #t)))))
             ((sge-event-key-released)
              (let ((released (vector-ref event 1)))
                (vector-set! *key-vec* released #f))))
           (poll-events)))))

(define (lpm-sleep)
  (let ((logic-usec (sge-timer-reset *logic-timer*)))
    (sge-micro-sleep (max 0 (- 2000 logic-usec)))))

(define (logic-loop)
  (cond ((not (sge-is-running?)) '())
   (else
    (logic-step (sge-timer-reset *delta-timer*))
    (poll-events)
    (cond (*low-power-mode* (lpm-sleep)))
    (logic-loop))))

(define *player* (Player))
(*player* 'init)

(sge-camera-set-target (*player* 'get-handle))
(sge-camera-set-springiness 1.5)
(sge-camera-set-zoom
 (let ((avg-screen (/ (+ (car (sge-window-size))
                         (cdr (sge-window-size))) 2)))
   (floor (* avg-screen (/ 1 585)))))

(switch-level apt-0)

(define (logic-step dt)
  (*current-level* 'update dt))
