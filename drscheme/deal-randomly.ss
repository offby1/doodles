(module deal-randomly mzscheme
  (require (lib "cards.ss" "games" "cards"))
  (require (all-except (lib "classes.ss" "games" "cards") pasteboard%))
  (require (lib "class.ss"))
  (require (lib "mred.ss" "mred"))
  (require "fys.ss")
  (define *d* (let ((d (make-deck)))
                (for-each (lambda (c)
                           (send c face-up))
                         d)
                d))

  (define *cw* (send (car *d*) card-width))
  (define *ch* (send (car *d*) card-height))

  (define (1- x)
    (- x 1))

  (define my-table%
    (class table% 
      (rename (o-s-c on-subwindow-char))
      (override on-subwindow-char)
      (define on-subwindow-char 
        (lambda (receiver event) 
          (when (eq? 'escape (send event get-key-code))
            
            ;; TODO -- figure out how to exit our loops.
            (printf "~S sez \"Escape!\"~%" receiver))
          
          (o-s-c receiver event)))
      (super-instantiate ())))

  (define *t* (make-object my-table% "snowball" 13 5))

  (define (card->value c)
    (+ (* 13
          (1- (send c get-suit-id)))
       (* 1
          (1- (send c get-value)))))

  (define *initial-region* (make-region
                            0
                            (- (send *t* table-height)
                               (send (car *d*) card-height))
                            (send *t* table-width)
                            (send (car *d*) card-height)
                            "unshuffled deck goes here" #f))
  (define (card->initial-position c)
    (list
     (+
      (region-x *initial-region*)
      (* (- (region-w
             *initial-region*)
            (/ *cw* 2)) 
         (/
          (card->value c) 52)))
     (region-y *initial-region*)))
  (send *t* add-region *initial-region*)
  ;; put each card on the table in its initial position.
  (send *t* add-cards *d* 0 0 (lambda (index) (apply values (card->initial-position (list-ref *d* index)))))

  ;;(send *t* add-cards-to-region *d* *initial-region*)

  ;; for debugging
  (send *t* set-single-click-action
        (lambda (c)
          (printf "The ~A of ~A~%"
                  (let ((v (send c get-value)))
                    (case v
                      ;; it's a shame that MzScheme's `format' doesn't
                      ;; support the ~R control.
                      ((1) 'ace)
                      ((2) 'two)
                      ((3) 'three)
                      ((4) 'four)
                      ((5) 'five)
                      ((6) 'six)
                      ((7) 'seven)
                      ((8) 'eight)
                      ((9) 'nine)
                      ((10) 'ten)
                      ((11) 'jack)
                      ((12) 'queen)
                      ((13) 'king)))
                  (send c get-suit ))))

  (let ()
    (define menu-bar (instantiate menu-bar% () (parent *t*)))

    (define main-menu  (instantiate menu% () 
                                    (label "&Stuff" )
                                    (parent menu-bar)))

    (define quick-menu-item (instantiate checkable-menu-item% ()
                                         (label "&Quick")
                                         (parent main-menu)
                                         (callback (lambda (it ev) (void)))))
    
    
    (define-syntax quickly
      (syntax-rules ()
        ((_ body ...)
         (dynamic-wind
             (lambda () (if (send quick-menu-item is-checked?) (send *t* begin-card-sequence)))
             (lambda () body ... )
             (lambda () (if (send quick-menu-item is-checked?) (send *t* end-card-sequence)))))))

    (send menu-bar enable #t)
    (instantiate menu-item% ()
                 (label "&Deal Randomly")
                 (parent main-menu)
                 (callback (lambda (item event) 
                             (set! *d* (vector->list (fys! (apply vector *d*))))
                             (quickly
                              (let loop ((cards-moved 0)
                                         (d *d*))
                                (when (not (null? d))
                                  (let ((x (* *cw* (remainder cards-moved 13)))
                                        (y (* *ch* (quotient  cards-moved 13))))
                                    (send *t* move-card (car d)
                                          x
                                          y))
                                  (when (send (car d) face-down?)
                                    (send *t* card-face-up (car d) ))
                                  (loop (+ 1 cards-moved)
                                        (cdr d)))))
                             (printf "Done.\n"))))
    (instantiate menu-item% ()
                 (label "Send 'em Back &Home")
                 (parent main-menu)
                 (callback (lambda (item event)
                             (quickly
                              (for-each (lambda (c)
                                          (send/apply *t* move-card c 
                                                      (card->initial-position c)
                                                      ))
                                        (reverse *d*)))
                             )))

    (instantiate menu-item% ()
                 (label "&Exit")
                 (parent main-menu)
                 (callback (lambda (item event)
                             (exit))))
    (send *t* show #t)))
