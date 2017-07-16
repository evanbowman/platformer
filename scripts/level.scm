;;;
;;; level.scm
;;;

(define *current-level*
  (lambda (unused-args) '()))

(define room-alpha-dark 215)

(define (create-room x y w h)
  (let ((entity (entity-create)))
    (entity-set-animation entity anim-pixel)
    (entity-set-scale entity w h)
    (entity-set-position entity x y)
    (entity-set-zorder entity 3)
    (vector entity x y w h room-alpha-dark)))

(define (remove-room room)
  (let ((entity (vector-ref room 0)))
    (entity-remove entity)))

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
            (let ((x (vector-ref room 1))
                  (y (vector-ref room 2))
                  (w (vector-ref room 3))
                  (h (vector-ref room 4))
                  (a (vector-ref room 5)))
              (cond
               ((and (> (car player-pos) x)
                     (> (cdr player-pos) y)
                     (< (car player-pos) (+ w x))
                     (< (cdr player-pos) (+ h y)))
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
                    a)))))
          (entity-set-rgba (vector-ref room 0) 20 20 54
                           (floor (inexact->exact new-alpha)))
          (vector-set! room 5 new-alpha))
        room-list))))
   
  ((enter)
   (lambda ()
     (for-each (lambda (hook) (hook)) entry-hooks)))

  ((exit)
   (lambda ()
     (for-each (lambda (entity) (entity-remove entity)) entity-list)
     (for-each (lambda (room) (remove-room room)) room-list)
     (set! entity-list '())
     (for-each (lambda (hook) (hook)) exit-hooks))))

(define (switch-level new-level)
  (let ((old-level *current-level*))
    (set! *current-level* new-level)
    (old-level 'exit)
    (new-level 'enter)
    old-level))

(define apartment-0 (Level))

(apartment-0
 'add-entry-hook
 (lambda ()
   (define bkg (entity-create))
   (define fg (entity-create))
   (define sunbeam-0 (entity-create))
   (define sunbeam-1 (entity-create))
   (define lamp-light (entity-create))
   (set-refresh-rgba 20 20 54 255)
   (entity-set-rgba lamp-light 255 255 255 255)
   (entity-set-animation bkg anim-apt-0-bkg)
   (entity-set-animation sunbeam-0 anim-sunbeam-0)
   (entity-set-animation sunbeam-1 anim-sunbeam-1)
   (entity-set-animation lamp-light anim-ceiling-light-0)
   (entity-set-animation fg anim-apt-0-fg)
   (entity-set-zorder bkg -1)
   (entity-set-zorder sunbeam-0 1)
   (entity-set-zorder sunbeam-1 1)
   (entity-set-zorder lamp-light 1)
   (entity-set-zorder fg 2)
   (entity-set-blend-mode sunbeam-0 blend-add)
   (entity-set-blend-mode sunbeam-1 blend-add)
   (entity-set-blend-mode lamp-light blend-add)
   (entity-set-position sunbeam-0 161 18)
   (entity-set-position sunbeam-1 214 66)
   (entity-set-position fg -15 -16)
   (entity-set-position lamp-light 343 78)
   (apartment-0 'add-entity bkg)
   (apartment-0 'add-entity sunbeam-0)
   (apartment-0 'add-entity sunbeam-1)
   (apartment-0 'add-entity lamp-light)
   (apartment-0 'add-entity fg)
   (*player* 'reset-with-position 364 119)
   (camera-set-center 365 119)
   (apartment-0 'add-room (create-room 0 0 64 144))
   (apartment-0 'add-room (create-room 64 0 196 144))
   (apartment-0 'add-room (create-room 261 0 192 144))))

(define apartments-hallway (Level))

