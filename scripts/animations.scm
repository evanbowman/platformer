;;;
;;; animations.scm
;;;
;;; All the handles to animations used by the game.
;;;

(define anim-player-run
  (sge-animation-create
   "player-run-cycle.png"
   0 0 21 44 10 20))

(define anim-apt-0-bkg
  (sge-animation-create
   "apartment-level-0.png"
   0 0 448 144 0 0))

(define anim-apt-0-fg
  (sge-animation-create
   "apartment-level-0-mask.png"
   0 0 480 176 0 0))

(define anim-sunbeam-0
  (sge-animation-create
   "sunbeam-0.png"
   0 0 96 99 0 0))

(define anim-sunbeam-1
  (sge-animation-create
   "sunbeam-1.png"
   0 0 43 44 0 0))

(define anim-ceiling-light-0
  (sge-animation-create
   "ceiling-light-0.png"
   0 0 83 50 0 0))

(define anim-pixel
  (sge-animation-create
   "pixel.png"
   0 0 1 1 0 0))

(define anim-ubuntu-mono-18
  (sge-animation-create
   "ubuntu-mono-18.png"
   0 0 9 18 0 0))

;; (define anim-apt-hallway
;;   (sge-animation-create
;;    "apartment-hallway.png"
;;    ...))
