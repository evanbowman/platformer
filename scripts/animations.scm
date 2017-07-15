;;;
;;; animations.scm
;;;
;;; All the handles to animations used by the game.
;;;

(define anim-player-run
  (animation-create
   "player-run-cycle.png"
   0 0 21 44 10 20))

(define anim-apt-0-bkg
  (animation-create
   "apartment-level-0.png"
   0 0 384 144 0 0))

(define anim-apt-0-fg
  (animation-create
   "apartment-level-0-mask.png"
   0 0 414 176 0 0))

(define anim-sunbeam-0
  (animation-create
   "sunbeam-0.png"
   0 0 96 99 0 0))

(define anim-sunbeam-1
  (animation-create
   "sunbeam-1.png"
   0 0 43 44 0 0))

(define anim-ceiling-light-0
  (animation-create
   "ceiling-light-0.png"
   0 0 83 50 0 0))

(define anim-pixel
  (animation-create
   "pixel.png"
   0 0 1 1 0 0))
