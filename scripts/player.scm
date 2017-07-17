;;;
;;; player.scm
;;;
;;; Player update logic script.
;;;
;;; The Player logic is basically a big state machine. Should
;;; be pretty easy to follow how it works.
;;;

(class Player
  ((entity-handle (sge-entity-create))
   (anim-timer 0)
   (keyframe 0)
   (state 'idle)
   (x-pos 0)
   (y-pos 0)
   (x-speed 0.0)
   (y-speed 0.0))

  ((init)
   (lambda ()
     (sge-entity-set-animation entity-handle anim-player-run)))

  ((get-handle)
   (lambda () entity-handle))
  
  ((reset-with-position)
   (lambda (x y)
     (set! x-pos x)
     (set! y-pos y)
     (set! x-speed 0)
     (set! y-speed 0)
     (set! state 'idle)
     (set! keyframe 0)
     (sge-entity-set-position entity-handle x y)
     (sge-entity-set-animation entity-handle anim-player-run)
     (sge-entity-set-keyframe entity-handle 0)))

  ((get-position)
   (lambda () (cons x-pos y-pos)))

  ((set-position)
   (lambda (x y)
     (set! x-pos x)
     (set! y-pos y)
     (sge-entity-set-position entity-handle x y)))
  
  ((update)
   (lambda (dt)
     (define (run-impl stop-key)
       (cond ((not (sge-key-pressed? stop-key))
              (set! state 'idle)
              (set! keyframe 0)
              (sge-entity-set-keyframe entity-handle 0)))
       (cond
        ((> anim-timer 60000)
         (set! anim-timer 0)
         (set! keyframe (if (< keyframe 10) (+ keyframe 1) 4))
         (sge-entity-set-keyframe entity-handle keyframe))
        (else (set! anim-timer (+ anim-timer dt))))
       (set! x-speed (lerp (* 0.35 (sgn x-speed)) x-speed
                           (* dt 0.0000020))))
     
     (case state
       ((run-left)
        (run-impl mapped-key-left))

       ((run-right)
        (run-impl mapped-key-right))
       
       ((idle)
        (cond
         ((sge-key-pressed? mapped-key-left)
          (set! state 'run-left)
          (set! x-speed -0.01)
          (sge-entity-set-scale entity-handle -1.0 1.0))
         ((sge-key-pressed? mapped-key-right)
          (set! state 'run-right)
          (set! x-speed 0.01)
          (sge-entity-set-scale entity-handle 1.0 1.0)))
        (set! x-speed (lerp 0.000 x-speed (* dt 0.000015))))

       ((sitting)
        '())

       ((sit-down)
        '())

       ((stand-up)
        '())
       
       ((jumping)
        '()))
     (let ((rate-factor (* dt 0.001)))
       (set! x-pos (+ x-pos (* x-speed rate-factor)))
       (set! y-pos (+ y-pos (* y-speed rate-factor)))
       (sge-entity-set-position entity-handle x-pos y-pos))))
  
  ((remove)
   (lambda ()
     (sge-entity-remove entity-handle))))
