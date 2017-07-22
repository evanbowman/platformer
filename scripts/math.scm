;;;
;;; math.scm
;;;
;;; Commonly useful math functions for game programming.
;;;

(define (clamp x floor ceil)
  (cond
   ((< x floor) floor)
   ((> x ceil) ceil)
   (else x)))

(define (lerp a b t)
  (+ (* a t) (* b (- 1.0 t))))

(define (smoothstep a b x)
  (set! x (clamp (/ (- x a) (- b a)) 0.0 1.0))
  (* x (* x (- 3 (* 2 x)))))

(define (distance p1 p2)
  (let ((x1 (car p1)) (x2 (car p2))
        (y1 (cdr p1)) (y2 (cdr p2)))
    (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))))

(define (sgn num)
  (if (> num 0) 1.0 -1.0))
