;;;
;;; level-manager.scm
;;;

(define *lvl-current* (lambda (unused-args) '()))

(define (lvl-set! level)
  (set! *lvl-current* level))

(define (restore-level-defaults)
  (sge-camera-set-target (*player* 'get-handle))
  (sge-camera-set-springiness 1.5)
  (sge-camera-set-zoom
   (let ((avg-screen (/ (+ (car (sge-window-size))
                           (cdr (sge-window-size))) 2)))
     (floor (* avg-screen (/ 1.0 585.0))))))

(define (lvl-switch symbol)
  (*lvl-current* 'exit)
  (set! *lvl-current* '())
  (restore-level-defaults)
  (load (string-append *resource-path* "scripts/levels/" (symbol->string symbol) ".scm"))
  (cond
   ((null? *lvl-current*)
    (error "level plugin did not set the current level"))))
