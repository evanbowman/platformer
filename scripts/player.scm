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
   (pos (cons 0.0 0.0))
   (speed (cons 0.0 0.0)))

  ((init)
   (lambda ()
     (sge-entity-set-animation entity-handle (anim-ref 'anim-player-run))))

  ((get-handle)
   (lambda () entity-handle))
  
  ((reset-with-position)
   (lambda (new-pos)
     (set! pos new-pos)
     (set! speed (cons 0.0 0.0))
     (set! state 'idle)
     (set! keyframe 0)
     (sge-entity-set-position entity-handle new-pos)
     (sge-entity-set-animation entity-handle (anim-ref 'anim-player-run))
     (sge-entity-set-keyframe entity-handle 0)))

  ((get-position)
   (lambda () pos))

  ((set-position)
   (lambda (new-pos)
     (set! pos new-pos)
     (sge-entity-set-position entity-handle new-pos)))
  
  ((update)
   (lambda (dt)
     (define (run-impl stop-key)
       (cond ((not (vector-ref *key-vec* stop-key))
              (set! state 'idle)
              (set! keyframe 0)
              (sge-entity-set-keyframe entity-handle 0)))
       (cond
        ((> anim-timer 60000)
         (set! anim-timer 0)
         (set! keyframe (if (< keyframe 10) (+ keyframe 1) 4))
         (sge-entity-set-keyframe entity-handle keyframe))
        (else (set! anim-timer (+ anim-timer dt))))
       (set-car! speed (lerp (* 0.35 (sgn (car speed))) (car speed)
                             (* dt 0.0000020))))
     
     (case state
       ((run-left)
        (run-impl mapped-key-left))

       ((run-right)
        (run-impl mapped-key-right))
       
       ((idle)
        (cond
         ((vector-ref *key-vec* mapped-key-left)
          (set! state 'run-left)
          (set-car! speed -0.01)
          (sge-entity-set-scale entity-handle (cons -1.0 1.0)))
         ((vector-ref *key-vec* mapped-key-right)
          (set! state 'run-right)
          (set-car! speed 0.01)
          (sge-entity-set-scale entity-handle (cons 1.0 1.0))))
        (set-car! speed (lerp 0.000 (car speed) (* dt 0.000015))))

       ((sitting)
        '())

       ((sit-down)
        '())

       ((stand-up)
        '())
       
       ((jumping)
        '()))
     (let ((rate-factor (* dt 0.001)))
       (set-car! pos (+ (car pos) (* (car speed) rate-factor)))
       (set-cdr! pos (+ (cdr pos) (* (cdr speed) rate-factor)))
       (sge-entity-set-position entity-handle pos))))
  
  ((remove)
   (lambda ()
     (sge-entity-remove entity-handle))))

(define *player* (Player))
(*player* 'init)
