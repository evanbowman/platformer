
((lambda ()
   (define apt-0 (Level))
   (define bkg (apt-0 'make-entity))
   (define fg (apt-0 'make-entity))
   (define sunbeam-0 (apt-0 'make-entity))
   (define sunbeam-1 (apt-0 'make-entity))
   (define lamp-light (apt-0 'make-entity))
   (sge-set-refresh-rgba #(20 20 54 255))
   (sge-entity-set-position sunbeam-0 (cons 161.0 18.0))
   (sge-entity-set-position sunbeam-1 (cons 214.0 66.0))
   (sge-entity-set-position fg (cons -15.0 -16.0))
   (sge-entity-set-position lamp-light (cons 343.0 78.0))
   (sge-entity-set-animation bkg (anim-ref 'anim-apt-0-bkg))
   (sge-entity-set-animation sunbeam-0 (anim-ref 'anim-sunbeam-0))
   (sge-entity-set-animation sunbeam-1 (anim-ref 'anim-sunbeam-1))
   (sge-entity-set-animation lamp-light (anim-ref 'anim-ceil-lt-0))
   (sge-entity-set-animation fg (anim-ref 'anim-apt-0-fg))
   (sge-entity-set-zorder bkg -1)
   (sge-entity-set-zorder sunbeam-0 1)
   (sge-entity-set-zorder sunbeam-1 1)
   (sge-entity-set-zorder lamp-light 1)
   (sge-entity-set-zorder fg 2)
   (sge-entity-set-blend-mode sunbeam-0 sge-blend-add)
   (sge-entity-set-blend-mode sunbeam-1 sge-blend-add)
   (sge-entity-set-blend-mode lamp-light sge-blend-add)
   (*player* 'reset-with-position (cons 364.0 119.0))
   (sge-camera-set-center (cons 365.0 119.0))
   (sge-camera-set-target (*player* 'get-handle))
   (apt-0 'add-room (create-room (cons 0.0 0.0)
                                 (cons 64.0 144.0)))
   (apt-0 'add-room (create-room (cons 64.0 0.0)
                                 (cons 196.0 144.0)))
   (apt-0 'add-room (create-room (cons 261.0 0.0)
                                 (cons 192.0 144.0)))
   (lvl-set! apt-0)))
