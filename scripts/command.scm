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
       (sge-entity-create)
       sge-attrib-hidden)
      sge-attrib-position-absolute)
     anim-pixel)
    1000)
   0 0 0 100))

(define cmd-mark
  (cons
   (sge-entity-set-rgba
    (sge-entity-set-zorder
     (sge-entity-set-scale
      (sge-entity-clone cmd-box)
      1 18)
     1001)
    255 79 0 255)
   0))

(define cmd-box-height 25)
(define cmd-char-width 9)
(define cmd-char-height 18)
(define cmd-char-margin 3)

(define (cmd-mode)
  (define view-size (sge-camera-get-view-size))
  (set! cmd-hist-ptr -1)
  (sge-entity-remove-attrib cmd-box sge-attrib-hidden)
  (sge-entity-remove-attrib (car cmd-mark) sge-attrib-hidden)
  (cmd-set-mark-offset 0)
  (sge-entity-set-scale cmd-box (car view-size) cmd-box-height)
  (sge-entity-set-position cmd-box 0 (- (cdr view-size) cmd-box-height))
  (cmd-read)
  (cmd-clear-expr)
  (sge-entity-add-attrib cmd-box sge-attrib-hidden)
  (sge-entity-add-attrib (car cmd-mark) sge-attrib-hidden)
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

(define (cmd-get-char)
  (cond
   ((null? cmd-char-pool) (cmd-create-char))
   (else
    (let ((top-char (car cmd-char-pool)))
      (set! cmd-char-pool (cdr cmd-char-pool))
      top-char))))

(define (cmd-push-char char-code)
  (define char-entity (cmd-get-char))
  (sge-entity-remove-attrib char-entity sge-attrib-hidden)
  (let ((char-x (+ (* (length cmd-expr-visual) cmd-char-width)
                   cmd-char-margin))
        (char-y (- (cdr (sge-camera-get-view-size))
                   cmd-char-height cmd-char-margin)))
    (sge-entity-set-position char-entity char-x char-y))
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
        (else
         (let ((top (car cmd-expr-raw)))
           (set! cmd-expr-raw (cdr cmd-expr-raw))
           top))))

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

(define (cmd-set-mark-offset offset-from-expr-end)
  (define expr-len (length cmd-expr-raw))
  (define offset (min expr-len (max 0 offset-from-expr-end)))
  (define pos (- expr-len offset))
  (define view-height (cdr (sge-camera-get-view-size)))
  (sge-entity-set-position (car cmd-mark)
                           (+ cmd-char-margin (* pos cmd-char-width))
                           (- view-height cmd-char-height cmd-char-margin))
  (set-cdr! cmd-mark offset))

(define (cmd-consume)
  (define result (eval-string (cmd->string)))
  (cmd-push-history-norepeat)
  (cmd-clear-expr)
  (cmd-set-mark-offset 0)
  result)

(define cmd-hist-ptr -1)

(define cmd-cache '())

(define (cmd-replace new-cmd)
  (cond ((null? new-cmd) '())
        (else
         (cmd-push-char (car new-cmd))
         (cmd-replace (cdr new-cmd)))))

(define (cmd-restore-history history-item)
  (cmd-clear-expr)
  (cmd-replace (reverse (list-ref cmd-history history-item))))

(define (cmd-on-up-arrow)
  (define old-expr-len (length cmd-expr-raw))
  (cond
   ((< cmd-hist-ptr (- (length cmd-history) 1))
    (cond
     ((eq? cmd-hist-ptr -1)
      (set! cmd-cache cmd-expr-raw)))
    (set! cmd-hist-ptr (+ cmd-hist-ptr 1))
    (cmd-restore-history cmd-hist-ptr)))
  (cmd-set-mark-offset 0))

(define (cmd-on-down-arrow)
  (cond
   ((eq? cmd-hist-ptr -1) '())
   ((eq? cmd-hist-ptr 0)
    (cmd-clear-expr)
    (cmd-replace (reverse cmd-cache)))
   (else
    (cmd-restore-history (- cmd-hist-ptr 1))))
  (set! cmd-hist-ptr (max -1 (- cmd-hist-ptr 1)))
  (cmd-set-mark-offset 0))

(define (cmd-on-text-event text-char)
  (set! cmd-hist-ptr -1)
  (cond ((not (member text-char cmd-text-blacklist))
         (cond
          ((eq? (cdr cmd-mark) 0)
           (cmd-push-char text-char)
           (cmd-set-mark-offset (cdr cmd-mark)))
          (else
           (cmd-rebase-edit
            (lambda () (cmd-push-char text-char))
            (lambda () (cmd-set-mark-offset (cdr cmd-mark)))))))))

(define (cmd-rebase-edit expr-modifier mark-modifier)
  "Apply expr-modifier at the mark head, then apply mark-modifier."
  (let remove ((depth (cdr cmd-mark)) (replay-list '()))
    (cond
     ((eq? depth 0)
      (expr-modifier)
      (let replay ((r-list replay-list))
        (cond
         ((null? r-list)
          (mark-modifier))
         (else
          (cmd-push-char (car r-list))
          (replay (cdr r-list))))))
     (else
      (let ((expr-top (car cmd-expr-raw)))
        (cmd-pop-char)
        (remove (- depth 1) (cons expr-top replay-list)))))))

(define (cmd-on-backspace)
  (cond
   ((eq? (cdr cmd-mark) 0)
    (cmd-pop-char)
    (cmd-set-mark-offset 0))
   (else
    (let ((expr-len (length cmd-expr-raw)))
      (cond
       ((eq? expr-len (cdr cmd-mark)) '())
       (else
        (cmd-rebase-edit
         cmd-pop-char
         (lambda () (cmd-set-mark-offset (cdr cmd-mark))))))))))

(define (cmd-on-left-arrow)
  (cmd-set-mark-offset (+ (cdr cmd-mark) 1)))

(define (cmd-on-right-arrow)
  (cmd-set-mark-offset (- (cdr cmd-mark) 1)))

(define (cmd-read)
  (define continue #t)
  (define get-event
    (lambda ()
      (let ((event (sge-poll-events)))
        (cond ((null? event) '())
              (else
               (case (car event)
                 ((sge-event-text) (cmd-on-text-event (cdr event)))
                 ((sge-event-key-pressed)
                  (let ((pressed (cdr event)))
                    (cond
                     ((eq? pressed sge-key-esc) (set! continue #f))
                     ((eq? pressed sge-key-return) (cmd-consume))
                     ((eq? pressed sge-key-backspace) (cmd-on-backspace))
                     ((eq? pressed sge-key-up) (cmd-on-up-arrow))
                     ((eq? pressed sge-key-down) (cmd-on-down-arrow))
                     ((eq? pressed sge-key-left) (cmd-on-left-arrow))
                     ((eq? pressed sge-key-right) (cmd-on-right-arrow))))))
               (get-event))))))
  (get-event)
  (cond ((or (not continue)
             (not (sge-is-running?)))
         '())
        (else
         (sge-micro-sleep 5000)
         (cmd-read))))
