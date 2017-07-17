;;;
;;; command.scm
;;;
;;; Evaluate new code from the user. The procedure cmd-mode
;;; creates a text entry zone, sort of like the emacs minibuffer,
;;; where the user can input s-expressions to be evaluated.
;;;

(use-modules (rnrs bytevectors))

(define cmd-box
  (sge-entity-set-rgba
   (sge-entity-set-zorder
    (sge-entity-set-animation
     (sge-entity-add-attrib
      (sge-entity-add-attrib
       (sge-entity-create) sge-attrib-hidden)
      sge-attrib-position-absolute) anim-pixel) 1000)
   0 0 0 100))

(define cmd-box-height 25)
(define cmd-char-width 9)
(define cmd-char-height 18)
(define cmd-char-margin 3)

(define (cmd-mode)
  (define view-size (sge-camera-get-view-size))
  (set! cmd-hist-ptr -1)
  (sge-entity-remove-attrib cmd-box sge-attrib-hidden)
  (sge-entity-set-scale cmd-box (car view-size) cmd-box-height)
  (sge-entity-set-position cmd-box 0 (- (cdr view-size) cmd-box-height))
  (cmd-read)
  (sge-entity-add-attrib cmd-box sge-attrib-hidden)
  ;; If we don't reset the game's delta timers to ignore
  ;; elapsed time during text entry, the game will think that
  ;; a huge amount of time elapsed when in reality the game
  ;; was merely paused.
  (sge-timer-reset *delta-timer*)
  (sge-timer-reset *logic-timer*))

(define cmd-expr-raw '())
(define cmd-expr-visual '())
;; Maintain a pool of chars so we don't stupidly exhaust UUIDs when
;; creating tons of new character entitys.
(define cmd-char-pool '())

(define (cmd-create-char)
  (define char-entity (sge-entity-create))
  (sge-entity-set-animation char-entity anim-pixel)
  (sge-entity-set-scale char-entity cmd-char-width cmd-char-height)
  (sge-entity-set-animation char-entity anim-ubuntu-mono-18)
  (sge-entity-set-zorder char-entity 1001)
  (sge-entity-add-attrib char-entity sge-attrib-position-absolute)
  (sge-entity-set-rgba char-entity 200 200 200 255)
  char-entity)

(define (cmd-append-char char-code)
  (define char-entity (cond
                       ((null? cmd-char-pool) (cmd-create-char))
                       (else (let ((top-char (car cmd-char-pool)))
                               (set! cmd-char-pool (cdr cmd-char-pool))
                               top-char))))
  (sge-entity-remove-attrib char-entity sge-attrib-hidden)
  (sge-entity-set-position char-entity
                           (+ (* (length cmd-expr-visual) cmd-char-width)
                              cmd-char-margin)
                           (- (cdr (sge-camera-get-view-size))
                              cmd-char-height cmd-char-margin))
  (sge-entity-set-keyframe char-entity (- char-code 32))
  (set! cmd-expr-visual (cons char-entity cmd-expr-visual))
  (set! cmd-expr-raw (cons char-code cmd-expr-raw)))

(define (cmd-pop-char)
  (cond ((null? cmd-expr-visual) '())
        (else
         (set! cmd-char-pool (cons (car cmd-expr-visual) cmd-char-pool))
         (sge-entity-add-attrib (car cmd-expr-visual) sge-attrib-hidden)
         (set! cmd-expr-visual (cdr cmd-expr-visual))))
  (cond ((null? cmd-expr-raw) '())
        (else (set! cmd-expr-raw (cdr cmd-expr-raw)))))

(define (cmd-clear-expr)
  (cond ((null? cmd-expr-raw) '())
        (else
         (cmd-pop-char)
         (cmd-clear-expr))))

;; Some special input characters like backspace would otherwise
;; show up in the text event and be appended to the command expr.
(define cmd-text-blacklist
  (list
   8  ;; backspace
   10 ;; return
   ))

(define (cmd->string)
  (utf8->string (u8-list->bytevector (reverse cmd-expr-raw))))

(define cmd-history '())

(define (cmd-push-history)
  (set! cmd-history (cons cmd-expr-raw cmd-history)))

(define (cmd-push-history-norepeat)
  (cond
   ((null? cmd-history) (cmd-push-history))
   (else
    (cond
     ((equal? cmd-expr-raw (list-ref cmd-history 0)) '())
     (else (cmd-push-history))))))

(define (cmd-consume)
  (define result (eval-string (cmd->string)))
  (cmd-push-history-norepeat)
  (cmd-clear-expr)
  result)

(define cmd-hist-ptr -1)

(define cmd-cache '())

(define (cmd-replace new-cmd)
  (cond ((null? new-cmd) '())
        (else
         (cmd-append-char (car new-cmd))
         (cmd-replace (cdr new-cmd)))))

(define (cmd-restore-history history-item)
  (cmd-clear-expr)
  (cmd-replace (reverse (list-ref cmd-history history-item))))

(define (cmd-on-up-arrow)
  (cond ((< cmd-hist-ptr (- (length cmd-history) 1))
         (cond ((eq? cmd-hist-ptr -1)
                (set! cmd-cache cmd-expr-raw)))
         (set! cmd-hist-ptr (+ cmd-hist-ptr 1))
         (cmd-restore-history cmd-hist-ptr))))

(define (cmd-on-down-arrow)
  (cond ((eq? cmd-hist-ptr -1) '())
        ((eq? cmd-hist-ptr 0)
         (cmd-clear-expr)
         (cmd-replace (reverse cmd-cache)))
        (else (cmd-restore-history (- cmd-hist-ptr 1))))
  (set! cmd-hist-ptr (max -1 (- cmd-hist-ptr 1))))

(define (cmd-read)
  (define continue #t)
  (define get-event
    (lambda ()
      (let ((event (sge-poll-events)))
        (cond ((null? event) '())
              (else
               (case (car event)
                 ((sge-event-text)
                  (set! cmd-hist-ptr -1)
                  (cond ((not (member (cdr event) cmd-text-blacklist))
                         (cmd-append-char (cdr event)))))
                 ((sge-event-key-pressed)
                  (let ((pressed (cdr event)))
                    (cond
                     ((eq? pressed sge-key-esc) (set! continue #f))
                     ((eq? pressed sge-key-return) (cmd-consume))
                     ((eq? pressed sge-key-backspace) (cmd-pop-char))
                     ((eq? pressed sge-key-up) (cmd-on-up-arrow))
                     ((eq? pressed sge-key-down) (cmd-on-down-arrow))))))
               (get-event))))))
  (get-event)
  (cond ((or (not continue)
             (not (sge-is-running?)))
         (cmd-clear-expr))
        (else
         (sge-micro-sleep 5000)
         (cmd-read))))
