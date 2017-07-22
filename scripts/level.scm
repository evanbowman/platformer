;;;
;;; level.scm
;;;

(define *current-level*
  (lambda (unused-args) '()))

(define room-alpha-dark 215)

(define (create-room pos size)
  (let ((entity (sge-entity-create)))
    (sge-entity-set-animation entity (anim-ref 'anim-pixel))
    (sge-entity-set-scale entity size)
    (sge-entity-set-position entity pos)
    (sge-entity-set-zorder entity 3)
    (sge-entity-set-rgba entity (vector 20 20 54 room-alpha-dark))
    (vector entity pos size room-alpha-dark)))

(define (remove-room room)
  (let ((entity (vector-ref room 0)))
    (sge-entity-remove entity)))

(class Level
  ((entry-hooks '())
   (exit-hooks '())
   (entity-list '())
   (wall-list '())
   (room-list '()))

  ((add-entry-hook)
   (lambda (hook)
     (set! entry-hooks (cons hook entry-hooks))))
  
  ((add-exit-hook)
   (lambda (hook)
     (set! exit-hooks (cons hook exit-hooks))))

  ((add-entity)
   (lambda (entity)
     (set! entity-list (cons entity entity-list))))

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
                (if (< a (- room-alpha-dark 1))
                    (lerp room-alpha-dark a (* dt 0.000005))
                    room-alpha-dark)))))
          (sge-entity-set-rgba (vector-ref room 0)
                               (vector 20 20 54
                                       (floor (inexact->exact new-alpha))))
          (vector-set! room 3 new-alpha))
        room-list))))
   
  ((enter)
   (lambda ()
     (for-each (lambda (hook) (hook)) entry-hooks)))

  ((exit)
   (lambda ()
     (for-each (lambda (entity) (sge-entity-remove entity)) entity-list)
     (for-each (lambda (room) (remove-room room)) room-list)
     (set! entity-list '())
     (set! room-list '())
     (for-each (lambda (hook) (hook)) exit-hooks))))

(define (switch-level new-level)
  (let ((old-level *current-level*))
    (set! *current-level* new-level)
    (old-level 'exit)
    (new-level 'enter)
    old-level))

(define apt-0 (Level))

(apt-0
 'add-entry-hook
 (lambda ()
   (define bkg (sge-entity-create))
   (define fg (sge-entity-create))
   (define sunbeam-0 (sge-entity-create))
   (define sunbeam-1 (sge-entity-create))
   (define lamp-light (sge-entity-create))
   (sge-set-refresh-rgba #(20 20 54 255))
   (sge-entity-set-position sunbeam-0 (cons 161.0 18.0))
   (sge-entity-set-position sunbeam-1 (cons 214.0 66.0))
   (sge-entity-set-position fg (cons -15.0 -16.0))
   (sge-entity-set-position lamp-light (cons 343.0 78.0))
   (sge-entity-set-animation bkg (anim-ref 'anim-apt-0-bkg))
   (sge-entity-set-animation sunbeam-0 (anim-ref 'anim-sunbeam-0))
   (sge-entity-set-animation sunbeam-1 (anim-ref 'anim-sunbeam-1))
   (sge-entity-set-animation lamp-light (anim-ref 'anim-ceil-lt-0))
   ;; (sge-entity-set-rgba lamp-light 255 255 255 255)
   (sge-entity-set-animation fg (anim-ref 'anim-apt-0-fg))
   (sge-entity-set-zorder bkg -1)
   (sge-entity-set-zorder sunbeam-0 1)
   (sge-entity-set-zorder sunbeam-1 1)
   (sge-entity-set-zorder lamp-light 1)
   (sge-entity-set-zorder fg 2)
   (sge-entity-set-blend-mode sunbeam-0 sge-blend-add)
   (sge-entity-set-blend-mode sunbeam-1 sge-blend-add)
   (sge-entity-set-blend-mode lamp-light sge-blend-add)
   (apt-0 'add-entity bkg)
   (apt-0 'add-entity sunbeam-0)
   (apt-0 'add-entity sunbeam-1)
   (apt-0 'add-entity lamp-light)
   (apt-0 'add-entity fg)
   (*player* 'reset-with-position (cons 364.0 119.0))
   (sge-camera-set-center (cons 365.0 119.0))
   (apt-0 'add-room (create-room (cons 0.0 0.0)
                                 (cons 64.0 144.0)))
   (apt-0 'add-room (create-room (cons 64.0 0.0)
                                 (cons 196.0 144.0)))
   (apt-0 'add-room (create-room (cons 261.0 0.0)
                                 (cons 192.0 144.0)))))

(define apt-hallway (Level))

(apt-hallway
 'add-entry-hook
 (lambda ()
   (define bkg (sge-entity-create))
   ;(entity-set-animation bkg ...)
   (apt-hallway 'add-entity bkg)))
