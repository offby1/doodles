(module visible-shuffle mzscheme
  (require (lib "cards.ss" "games" "cards"))
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

  (define *t* (make-table "snowball" 13 5))
  
  (define (card->value c)
    (+ (* 13
          (1- (send c get-suit-id)))
       (* 1
          (1- (send c get-value)))))

  (define card->initpos
    (lambda (c)
      (values
       (* *cw* (1- (send c get-value  )))
       (* *ch* (1- (send c get-suit-id))))))
  
  (define (card->initpos* c)
    (call-with-values
        (lambda ()
          (card->initpos c))
      cons))

  ;; put each card on the table in its initial position.
  ;;(send *t* add-cards *d* 0 0 (lambda (index) (card->initpos (list-ref *d* index))))
  
  
  ;; the hell with that -- let's let Mr. Ed fan them out.
  (define initial-region (make-region
                          0
                          (- (send *t* table-height)
                             (send (car *d*) card-height))
                          (send *t* table-width)
                          (send (car *d*) card-height)
                          "unshuffled deck goes here" #f))
  (send *t* add-region initial-region)
  (send *t* add-cards-to-region *d* initial-region)
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
    (define swap-cards
      (let ((location-alist (map (lambda (c) (cons c (card->initpos* c)))
                                 *d*)))
        (lambda (c1 c2)

          ;; there's something fishy here.  I shouldn't have to maintain
          ;; the locations of the cards myself, and yet I cannot figure
          ;; out how to get the table to tell me where they are.

          ;; On second thought, though, it seems that what I'm
          ;; maintaining isn't "where the card is", but rather "where
          ;; I'd like the card to be".  It's certainly reasonable for
          ;; me to maintain *that* -- there's no way the system could
          ;; know where I want it to be.  In fact, I don't think
          ;; there's ever a case where I need to know a card's
          ;; location in pixels -- at most, I'd want to know what
          ;; region it's in, and I can presumably control that by
          ;; simply putting the card in some region or other.
          
          (letrec-values (((card->location move-and-update)
                           (values
                            (lambda (c)
                              (cdr (assq c location-alist)))
                            (lambda (c loc-pair)
                              (let ((newx (car loc-pair))
                                    (newy (cdr loc-pair)))
                                (send *t* move-card c newx newy)
                                (let ((old  (assq c location-alist)))
                                  (set-cdr! old (cons newx newy))))))))
                         (let ((loc-pair-1 (card->location c1)))
                           (move-and-update c1 (card->location c2))
                           (move-and-update c2 loc-pair-1))))))

    (define menu-bar (instantiate menu-bar% () (parent *t*)))
    (define main-menu  (instantiate menu% () 
                                    (label "Stuff" )
                                    (parent menu-bar)))
    (send menu-bar enable #t)
    (instantiate menu-item% ()
                 (label "&Shuffle")
                 (parent main-menu)
                 (callback (lambda (item event) 
                             (set! *d* (vector->list (fys! (apply vector *d*))))
                             (let loop ((cards-moved 0)
                                        (d *d*))
                               (when (not (null? d))
                                 (send *t* move-card (car d)
                                       (* *cw* (remainder cards-moved 13))
                                       (* *ch* (quotient  cards-moved 13)))
                                 (loop (+ 1 cards-moved)
                                       (cdr d))))
                             (printf "Done.\n"))))
    (instantiate menu-item% ()
                 (label "&Exit")
                 (parent main-menu)
                 (callback (lambda (item event)
                             (exit))))
    (send *t* show #t)))

