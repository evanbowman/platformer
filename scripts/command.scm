;;;
;;; command.scm
;;;
;;; Evaluate new code from the user. The procedure cmd-mode
;;; creates a text entry zone, sort of like the emacs minibuffer,
;;; where the user can input s-expressions to be evaluated.
;;;

(import (scheme file))

;; Required for procedure exception-message. Non-portable, obviously.
(import (chibi ast))

(define cmd-box (sge-entity-create))
(sge-entity-set-animation cmd-box (anim-ref 'anim-pixel))
(sge-entity-set-zorder cmd-box 1000)
(sge-entity-add-attrib cmd-box sge-attrib-hidden)
(sge-entity-add-attrib cmd-box sge-attrib-position-absolute)
(sge-entity-set-rgba cmd-box #(0 0 0 100))

(define cmd-mark
  (cons
   (sge-entity-set-rgba
    (sge-entity-set-zorder
     (sge-entity-set-scale
      (sge-entity-clone cmd-box)
      (cons 1.0 18.0))
     1001)
    #(255 79 0 255))
   0))

(define cmd-box-height 25.0)
(define cmd-char-width 9.0)
(define cmd-char-height 18.0)
(define cmd-char-margin 3.0)

(define (cmd-mode)
  (define view-size (sge-camera-get-viewsize))
  (set! cmd-hist-ptr -1)
  (sge-entity-remove-attrib cmd-box sge-attrib-hidden)
  (sge-entity-remove-attrib (car cmd-mark) sge-attrib-hidden)
  (cmd-set-mark-offset 0)
  (sge-entity-set-scale cmd-box (cons (car view-size) cmd-box-height))
  (sge-entity-set-position cmd-box (cons 0.0 (- (cdr view-size) cmd-box-height)))
  (cmd-read)
  (cmd-clear-expr)
  (sge-entity-add-attrib cmd-box sge-attrib-hidden)
  (sge-entity-add-attrib (car cmd-mark) sge-attrib-hidden))

(define *cmd-expr-raw* '())
(define *cmd-expr-visual* '())
;; Maintain a pool of chars so we don't stupidly exhaust UUIDs when
;; creating tons of new character entitys.
(define *cmd-char-pool* '())

(define (cmd-create-char)
  (define char-entity (sge-entity-create))
  (sge-entity-set-animation char-entity (anim-ref 'anim-ub-mono18))
  (sge-entity-set-zorder char-entity 1001)
  (sge-entity-add-attrib char-entity sge-attrib-position-absolute)
  char-entity)

(define (cmd-get-char)
  (cond
   ((null? *cmd-char-pool*) (cmd-create-char))
   (else
    (let ((top-char (car *cmd-char-pool*)))
      (set! *cmd-char-pool* (cdr *cmd-char-pool*))
      top-char))))

(define cmd-input-rgba #(200 200 200 255))
(define cmd-result-rgba #(150 150 150 255))

(define *cmd-current-rgba* cmd-input-rgba)

(define (cmd-push-char char-code)
  (define char-entity (cmd-get-char))
  (define expr-len (length *cmd-expr-visual*))
  (define view-size (sge-camera-get-viewsize))
  (cond
   ((< (* (+ expr-len 1) cmd-char-width) (car view-size))
    (sge-entity-remove-attrib char-entity sge-attrib-hidden)
    (sge-entity-set-position char-entity
                             (cons
                              (+ (* expr-len cmd-char-width)
                                 cmd-char-margin)
                              (- (cdr view-size)
                                 cmd-char-height cmd-char-margin)))
    (sge-entity-set-keyframe char-entity (- char-code 32))
    (sge-entity-set-rgba char-entity *cmd-current-rgba*)
    (set! *cmd-expr-visual* (cons char-entity *cmd-expr-visual*))
    (set! *cmd-expr-raw* (cons char-code *cmd-expr-raw*))
    char-entity)
   (else '())))

(define (cmd-pop-char)
  (cond ((null? *cmd-expr-visual*) '())
        (else
         (set! *cmd-char-pool* (cons (car *cmd-expr-visual*) *cmd-char-pool*))
         (sge-entity-add-attrib (car *cmd-expr-visual*) sge-attrib-hidden)
         (set! *cmd-expr-visual* (cdr *cmd-expr-visual*))))
  (cond ((null? *cmd-expr-raw*) '())
        (else
         (let ((top (car *cmd-expr-raw*)))
           (set! *cmd-expr-raw* (cdr *cmd-expr-raw*))
           top))))

(define (cmd-clear-expr)
  (cond ((null? *cmd-expr-raw*) '())
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

(define *cmd-history* '())

(define (cmd-save-history)
  (call-with-output-file (string-append *resource-path* ".cmd-history")
    (lambda (port)
      ;; Impose a history size limit to prevent startup lag
      (write (truncate *cmd-history* 250) port))))

(define (cmd-load-history)
  (define input-path (string-append *resource-path* ".cmd-history"))
  (cond
   ((file-exists? input-path)
    (call-with-input-file input-path
      (lambda (port) (set! *cmd-history* (read port)))))))

(define (cmd-push-history)
  (set! *cmd-history* (cons *cmd-expr-raw* *cmd-history*)))

(define (cmd-push-history-norepeat)
  (cond
   ((null? *cmd-history*) (cmd-push-history))
   (else
    (cond
     ((equal? *cmd-expr-raw* (list-ref *cmd-history* 0)) '())
     (else (cmd-push-history))))))

(define (cmd-set-mark-offset offset-from-expr-end)
  (define expr-len (length *cmd-expr-raw*))
  (define offset (min expr-len (max 0 offset-from-expr-end)))
  (define pos (- expr-len offset))
  (define view-height (cdr (sge-camera-get-viewsize)))
  (sge-entity-set-position (car cmd-mark)
                           (cons (+ cmd-char-margin (* pos cmd-char-width))
                                 (- view-height cmd-char-height cmd-char-margin)))
  (set-cdr! cmd-mark offset))

(define (cmd-consume)
  ;; call/cc because returning from an exception handler triggers
  ;; another exception, but we instead want to serialize the error
  ;; message and display it to the screen
  (call-with-current-continuation
   (lambda (return)
     (with-exception-handler
         (lambda (err)
           (cmd-clear-expr)
           (cmd-set-mark-offset 0)
           (return (exception-message err)))
       (lambda ()
         (let ((result (eval (read (open-input-string
                                    (int-list->string *cmd-expr-raw*))))))
           (cmd-push-history-norepeat)
           (cmd-clear-expr)
           (cmd-set-mark-offset 0)
           (return (->string result))))))))

(define cmd-hist-ptr -1)

(define cmd-cache '())

(define (cmd-replace new-cmd)
  (cmd-clear-expr)
  (let replace-impl ((cmd new-cmd))
    (cond ((null? cmd) '())
          (else
           (cmd-push-char (car cmd))
           (replace-impl (cdr cmd))))))

(define (cmd-restore-history history-item)
  (cmd-replace (reverse (list-ref *cmd-history* history-item))))

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
      (let ((expr-top (car *cmd-expr-raw*)))
        (cmd-pop-char)
        (remove (- depth 1) (cons expr-top replay-list)))))))

(define *cmd-displaying-result* #f)

(define (cmd-clear-if-displaying-result)
  (cond
   (*cmd-displaying-result*
    (cmd-clear-expr)
    (set! *cmd-displaying-result* #f))))

(define (cmd-on-up-arrow)
  (let ((old-expr-len (length *cmd-expr-raw*)))
    (cond
     ((< cmd-hist-ptr (- (length *cmd-history*) 1))
      (cond
       ((eq? cmd-hist-ptr -1)
        (set! cmd-cache *cmd-expr-raw*)))
      (set! cmd-hist-ptr (+ cmd-hist-ptr 1))
      (cmd-restore-history cmd-hist-ptr)))
    (cmd-set-mark-offset 0)))

(define (cmd-on-down-arrow)
  (cond
   ((eq? cmd-hist-ptr -1) '())
   ((eq? cmd-hist-ptr 0)
    (cmd-replace (reverse cmd-cache)))
   (else
    (cmd-restore-history (- cmd-hist-ptr 1))))
  (set! cmd-hist-ptr (max -1 (- cmd-hist-ptr 1)))
  (cmd-set-mark-offset 0))

(define (cmd-on-text-event text-char)
  (set! cmd-hist-ptr -1)
  (cond
   ((eq? (cdr cmd-mark) 0)
    (cmd-push-char text-char)
    (cmd-set-mark-offset (cdr cmd-mark)))
   (else
    (cmd-rebase-edit
     (lambda () (cmd-push-char text-char))
     (lambda () (cmd-set-mark-offset (cdr cmd-mark)))))))

(define (cmd-on-backspace)
  (cond
   ((eq? (cdr cmd-mark) 0)
    (cmd-pop-char)
    (cmd-set-mark-offset 0))
   (else
    (let ((expr-len (length *cmd-expr-raw*)))
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

(define (cmd-on-enter)
  (cond
   ((null? *cmd-expr-raw*) '())
   (else
    (set! *cmd-current-rgba* cmd-result-rgba)
    (let ((res-u8-list (string->int-list (cmd-consume))))
      (let ((view-width (car (sge-camera-get-viewsize))))
        (cmd-replace
         (cond
          ((> (* (length res-u8-list) cmd-char-width) view-width)
           (append
            (truncate res-u8-list
                      (inexact->exact
                       (round (- (/ view-width cmd-char-width) 4))))
            (list 46 46 46)))
          (else res-u8-list)))
        (set! *cmd-current-rgba* cmd-input-rgba)
        (set! cmd-hist-ptr -1)
        (set! *cmd-displaying-result* #t))))))

(define *cmd-ctrl-hotkey-alist*
  (list
   (cons sge-key-a
         (lambda ()
           (cmd-set-mark-offset (length *cmd-expr-raw*))))
   (cons sge-key-e
         (lambda ()
           (cmd-set-mark-offset 0)))))

(define (cmd-hotkey-apply key alist)
  (define resp (assq key alist))
  (case resp
    ((#f) '())
    (else ((cdr resp)))))

(define (cmd-on-ctrl-hotkey key)
  (cmd-hotkey-apply key *cmd-ctrl-hotkey-alist*))

(define (cmd-read)
  (define continue #t)
  (define get-event
    (lambda ()
      (let ((event (sge-poll-event)))
        (cond ((null? event) '())
              (else
               (let ((event-code (vector-ref event 0)))
                 (cond
                  ((eq? event-code sge-event-text-entered)
                   (let ((text-char (vector-ref event 1)))
                     (cond
                      ((not (member text-char cmd-text-blacklist))
                       (cmd-clear-if-displaying-result)
                       (cmd-on-text-event text-char)))))
                  ((eq? event-code sge-event-key-pressed)
                   (cmd-clear-if-displaying-result)
                   (let ((key (vector-ref event 1)))
                     (vector-set! *key-vec* key #t)
                     (cond
                      ((or (vector-ref *key-vec* sge-key-lctrl)
                           (vector-ref *key-vec* sge-key-rctrl))
                       (cmd-on-ctrl-hotkey key))
                      (else
                       (cond
                        ((eq? key sge-key-esc) (set! continue #f))
                        ((eq? key sge-key-return) (cmd-on-enter))
                        ((eq? key sge-key-backspace) (cmd-on-backspace))
                        ((eq? key sge-key-up) (cmd-on-up-arrow))
                        ((eq? key sge-key-down) (cmd-on-down-arrow))
                        ((eq? key sge-key-left) (cmd-on-left-arrow))
                        ((eq? key sge-key-right) (cmd-on-right-arrow)))))))
                  ((eq? event-code sge-event-key-released)
                   (vector-set! *key-vec* (vector-ref event 1) #f))))
               (get-event))))))
  (get-event)
  (cond ((or (not continue)
             (not (sge-is-running?)))
         '())
        (else
         (sge-microsleep 5000)
         (cmd-read))))
