;;;
;;; level.scm
;;;

(define *current-level*
  (lambda (unused-args) '()))

(define (create-room x y w h)
  (let ((entity (entity-create))
        (default-alpha 0))
    (entity-set-animation entity anim-pixel)
    (entity-set-scale entity w h)
    (entity-set-position entity x y)
    (entity-set-zorder entity 2)
    (vector entity x y w h default-alpha)))

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
                (lerp 0 a (* dt 0.0000025)))
               (else
                (lerp 215 a (* dt 0.000005))))))
          (cond
           ((not (and (eq? new-alpha 0) (eq? new-alpha 215)))
            (entity-set-rgba (vector-ref room 0) 20 20 54
                             (floor (inexact->exact new-alpha)))
            (vector-set! room 5 new-alpha))))
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
   (set-refresh-rgba 20 20 54 255)
   (entity-set-rgba (*player* 'get-handle) 255 255 255 128)
   (entity-set-animation bkg anim-apt-0-bkg)
   (entity-set-animation sunbeam-0 anim-sunbeam-0)
   (entity-set-animation sunbeam-1 anim-sunbeam-1)
   (entity-set-animation fg anim-apt-0-fg)
   (entity-set-zorder sunbeam-0 1)
   (entity-set-zorder sunbeam-1 1)
   (entity-set-zorder fg 3)
   (entity-set-blend-mode sunbeam-0 blend-add)
   (entity-set-blend-mode sunbeam-1 blend-add)
   (entity-set-position sunbeam-0 97 18)
   (entity-set-position sunbeam-1 150 66)
   (entity-set-position fg -15 -16)
   (apartment-0 'add-entity bkg)
   (apartment-0 'add-entity sunbeam-0)
   (apartment-0 'add-entity sunbeam-1)
   (apartment-0 'add-entity fg)
   (*player* 'reset-with-position 87 119)
   (camera-set-center 87 119)
   (apartment-0 'add-room (create-room 0 0 193 144))))
