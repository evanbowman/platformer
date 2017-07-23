;;;
;;; anim-manager.scm
;;;
;;; Management for animations used by the game.
;;; The animation manager loads textures lazily for
;;; improved startup times.
;;;

(define (texture-path fname)
  (string-append *resource-path* "textures/" fname))

(define *anims-loaded-alist* '())

(define *anims-unloaded-alist*
  '((anim-player-run    #("player-run-cycle.png" 0 0 21 44 10.0 20.0))
    (anim-apt-0-bkg     #("apartment-level-0.png" 0 0 448 144 0.0 0.0))
    (anim-apt-0-fg      #("apartment-level-0-mask.png" 0 0 480 176 0.0 0.0))
    (anim-sunbeam-0     #("sunbeam-0.png" 0 0 96 99 0.0 0.0))
    (anim-sunbeam-1     #("sunbeam-1.png" 0 0 43 44 0.0 0.0))
    (anim-ceil-lt-0     #("ceiling-light-0.png" 0 0 83 50 0.0 0.0))
    (anim-pixel         #("pixel.png" 0 0 1 1 0.0 0.0))
    (anim-ub-mono18     #("ubuntu-mono-18.png" 0 0 9 18 0.0 0.0))
    (anim-sunsetgrad    #("sunset-grad.png" 0 0 300 300 0.0 0.0))
    (anim-sunsetbldg1   #("sunset-bldg1.png" 0 0 321 506 0.0 506.0))
    (anim-sunsetskyline #("sunset-skyline.png" 0 0 346 322 346.0 280.0))))

(define (anim-ref symbol)
  (let ((found-loaded (assq symbol *anims-loaded-alist*)))
    (case found-loaded
      ((#f)
       (anim-load symbol))
      (else
       (cdr found-loaded)))))

(define (anim-load symbol)
  (let ((found-unloaded (assq symbol *anims-unloaded-alist*)))
    (case found-unloaded
      ((#f)
       (error "animation lookup failed" symbol))
      (else
       (let ((params (car (cdr found-unloaded))))
         (let ((new-anim
                (sge-animation-create
                 (texture-path (vector-ref params 0))
                 (cons (vector-ref params 1) (vector-ref params 2))
                 (cons (vector-ref params 3) (vector-ref params 4))
                 (cons (vector-ref params 5) (vector-ref params 6)))))
           (set! *anims-loaded-alist*
             (cons (cons symbol new-anim) *anims-loaded-alist*))
           (set! *anims-unloaded-alist* (del-assq symbol *anims-unloaded-alist*))
           new-anim))))))
