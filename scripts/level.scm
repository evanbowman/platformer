;;;
;;; level.scm
;;;

(define room-dark-alpha 215)

(define (create-room pos size)
  (let ((entity (sge-entity-create)))
    (sge-entity-set-animation entity (anim-ref 'anim-pixel))
    (sge-entity-set-scale entity size)
    (sge-entity-set-position entity pos)
    (sge-entity-set-zorder entity 3)
    (sge-entity-set-rgba entity (vector 20 20 54 room-dark-alpha))
    (vector entity pos size room-dark-alpha)))

(define (remove-room room)
  (let ((entity (vector-ref room 0)))
    (sge-entity-remove entity)))

(class Level
  ((entry-hooks '())
   (exit-hooks '())
   (update-hooks '())
   (entity-list '())
   (room-list '()))

  ((add-entry-hook)
   (lambda (hook)
     (set! entry-hooks (cons hook entry-hooks))))
  
  ((add-exit-hook)
   (lambda (hook)
     (set! exit-hooks (cons hook exit-hooks))))

  ((add-update-hook)
   (lambda (hook)
     (set! update-hooks (cons hook update-hooks))))
  
  ((make-entity)
   (lambda ()
     (define entity (sge-entity-create))
     (set! entity-list (cons entity entity-list))
     entity))

  ((add-room)
   (lambda (room)
     (set! room-list (cons room room-list))))
  
  ((update)
   (lambda (dt)
     (*player* 'update dt)
     (let ((player-pos (*player* 'get-position)))
       (for-each
        (lambda (room)
          (define new-alpha
            (let ((pos (vector-ref room 1))
                  (size (vector-ref room 2))
                  (a (vector-ref room 3)))
              (cond
               ((and (> (car player-pos) (car pos))
                     (> (cdr player-pos) (cdr pos))
                     (< (car player-pos) (+ (car size) (car pos)))
                     (< (cdr player-pos) (+ (cdr size) (cdr pos))))
                #| This 1 alpha unit margin prevents an
                annoying flicker that occurs due to round off
                error. |#                 
                (if (> a 1)
                    (lerp 0 a (* dt 0.0000025))
                    a))
               (else
                ;; See above comment about round off error.
                (if (< a (- room-dark-alpha 1))
                    (lerp room-dark-alpha a (* dt 0.000005))
                    room-dark-alpha)))))
          (sge-entity-set-rgba (vector-ref room 0)
                               (vector 20 20 54
                                       (floor (inexact->exact new-alpha))))
          (vector-set! room 3 new-alpha))
        room-list)
       (for-each (lambda (update-hook) (update-hook dt)) update-hooks))))
   
  ((enter)
   (lambda ()
     (for-each (lambda (hook) (hook)) entry-hooks)))

  ((exit)
   (lambda ()
     (for-each (lambda (entity)
                 (sge-entity-remove entity)) entity-list)
     (for-each (lambda (room) (remove-room room)) room-list)
     (set! entity-list '())
     (set! room-list '())
     (for-each (lambda (hook) (hook)) exit-hooks))))
