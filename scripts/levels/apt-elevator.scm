
((lambda ()
   (define apt-elevator (Level))
   (define camera-holder (apt-elevator 'make-entity))
   (define bkg-grad (apt-elevator 'make-entity))
   (define bkg-bldg1 (apt-elevator 'make-entity))
   (define view-size (sge-camera-get-viewsize))
   (define bkg-skyline (apt-elevator 'make-entity))
   (define state 'enter)
   (*player* 'set-position (cons -600.0 -600.0))
   (sge-camera-set-target camera-holder)
   (sge-camera-set-springiness 0.0)
   (sge-entity-set-position camera-holder (cons 0.0 (/ (cdr view-size) 2.0)))
   (sge-camera-set-center (cons 0.0 0.0))
   (sge-entity-set-animation bkg-grad (anim-ref 'anim-sunsetgrad))
   (sge-entity-set-animation bkg-bldg1 (anim-ref 'anim-sunsetbldg1))
   (sge-entity-set-animation bkg-skyline (anim-ref 'anim-sunsetskyline))
   (sge-entity-set-position bkg-bldg1 (cons (/ (car view-size) -2.0)
                                            (* (cdr view-size) (/ 4.0 3.0))))
   (sge-entity-set-position bkg-skyline
                            (cons (/ (car view-size) 2.0)
                                  (/ (cdr view-size) 2.0)))
   (sge-entity-set-zorder bkg-grad -4)
   (sge-entity-set-zorder bkg-skyline -3)
   (sge-entity-set-zorder bkg-bldg1 -1)
   (sge-entity-set-scale bkg-grad (cons
                                   (/ (car view-size) 300.0)
                                   (/ (cdr view-size) 300.0)))
   (sge-entity-add-attrib bkg-grad sge-attrib-position-absolute)
   (lvl-set! apt-elevator)

   (apt-elevator
    'add-update-hook
    (lambda (dt)
      (case state
        ((elevator-descend)
         (cond
          ((vector-ref *key-vec* sge-key-b)
           (set! state 'enter))))
        
        ((enter)
         (let ((bldg1-pos (sge-entity-get-position bkg-bldg1)))
           (sge-entity-set-position
            bkg-bldg1 (cons (car bldg1-pos)
                            (lerp (cdr view-size) (cdr bldg1-pos)
                                  (* dt 0.0000006))))
           (let ((progress (/ (cdr view-size) (cdr bldg1-pos))))
             ;; FIXME: This block sometimes passes non-floats to
             ;; entity-set-position... why?
             (sge-entity-set-position
              bkg-skyline (cons (/ (car view-size) 2.0)
                                (+ (/ (cdr view-size) 2.0)
                                   (* (/ (cdr view-size) 6.0) progress))))
             (let ((color (vector 255 255 255
                                  (inexact->exact (round (* 255 progress))))))
               (sge-entity-set-rgba bkg-bldg1 color)
               (sge-entity-set-rgba bkg-skyline color))
             (cond
              ((> progress 0.99)
               (set! state 'chapter-text-in))))))
        ((chapter-text-in)
         '()))))))
