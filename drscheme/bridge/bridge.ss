;; TODO:

;; find a better way to chatter than printf.  Perhaps make a scrolling
;; console that captures standard out.  (How nerdly.)

;; figure out how to draw cards oriented horizontally.  I posted to
;; plt-scheme@list.cs.brown.edu on May 10 asking this.

;; figure out how to deliver a stand-alone executable (see the dox for
;; `write-image-to-file' in the mzscheme manual).  The executable
;; created by `mzc --gui-exe bridge bridge.ss' is *almost* standalone,
;; but it loads something (probably bitmaps) from games/cards at
;; startup.  I thought I read somewhere that one could save a snapshot
;; of a running DrScheme image, which sounds promising; alas, I can't
;; find the reference.

(module bridge mzscheme
  (require (lib "cards.ss" "games" "cards"))
  (require (all-except (lib "classes.ss" "games" "cards") pasteboard%))
  (require (lib "class.ss"))
  (require (lib "mred.ss" "mred"))
  (require (lib "list.ss"))
  (require "rotate.ss")

  ;; this rename looks dumb -- hell, it *is* dumb -- but it's the only
  ;; way I can think of to avoid a name collision -- apparently 1.ss
  ;; and list.ss both define some of the same names (such as "second",
  ;; "third", etc.)
  (require (rename (lib "1.ss" "srfi") iota iota))

  (define *d* (make-deck))

  (define *cw* (send (car *d*) card-width))
  (define *ch* (send (car *d*) card-height))

  ;; TODO: think of a more elegant way to do this
  (define *quit-flag* #f)
  
  (define-syntax quickly
    (syntax-rules ()
      ((_ body ...)
       (dynamic-wind
           (lambda () (send *t* begin-card-sequence))
           (lambda () body ... )
           (lambda () (send *t* end-card-sequence))))))

  (define my-table%
    (class table% 
      (rename (o-s-c on-subwindow-char))
      (override on-subwindow-char)
      (inherit move-card)
      (public move-card-center)
      (define on-subwindow-char 
        (lambda (receiver event) 
          (when (eq? 'escape (send event get-key-code))
            (set! *quit-flag* #t))
          
          (o-s-c receiver event)))

      (define move-card-center
        (lambda (card x y)
          (move-card card 
                     (- x (/ *cw* 2))
                     (- y (/ *ch* 2)))))

      (super-instantiate ())))

  (define *t* #f)
  (let ((width-in-pixels (+ (* 2 *ch*)
                            (* 13/2 *cw*))))
    (set! *t* (make-object my-table% "The Snowball Bridge Club" 
                           (/ width-in-pixels *cw*)
                           (/ width-in-pixels *ch*))))
  
  (send *t* set-button-action 'left   'drag-raise/one)
  (send *t* set-button-action 'middle 'drag-raise/one)
  (send *t* set-button-action 'right  'drag-raise/one)

  (define *region-length* (- (send *t* table-width) (* 2 *ch*)))

  (define-struct player (region cards choose-card! card-home-location-proc))

  (define (make-card-removing-proc choice-proc)
    (lambda (p)
      (let ((choice (choice-proc (player-cards p))))
        (set-player-cards! p (remove choice (player-cards p)))
        choice)))
  
  (define dumbest-choose-card car)

  (define (random-choose-card hand)
    (list-ref hand (random (length hand))))

  (define interactively-choose-card
    (let ((choice #f))
      (lambda (hand)
        (let ((sem (make-semaphore)))
      
          ;; Doesn't this clobber any existing double-click-action?  Isn't
          ;; that a Bad Thing?
          (send *t* set-double-click-action 
                (lambda (card)
                  (if (member card hand)
                      (begin
                        (set! choice card)
                        (semaphore-post sem))
                    (begin
                      (bell)
                      (printf "You gotta click your own cards.~%")))
                  ))
          (yield sem)
          (unless choice
            (error "Looks like I don't understand semaphores."))
          choice))))

  (define *player-alist*
    (map (lambda (compass-direction quarter-turns choice-func)

           (cons compass-direction
                 (let* ((rotate-region (let ((table-middle (make-point (/ (send *t* table-width) 2)
                                                                       (/ (send *t* table-width) 2))))
                                         (lambda (r)
                                           (rotate-region r  table-middle
                                                          quarter-turns))))
                        (north-region (make-region *ch* 0
                                                   *region-length*
                                                   *ch*
                                                   (symbol->string
                                                    compass-direction) #f))
                        (r (rotate-region north-region)))
                   (make-player r
                                #f      ; cards -- this gets filled
                                        ; in when we deal
                                choice-func
                                (lambda (index)
                                  (let ((r (rotate-region
                                            (make-region
                                             (+ (region-x north-region) (* (/ (region-w north-region) 14) index))
                                             (region-y north-region)
                                             *cw*
                                             *ch*
                                             #f
                                             #f
                                             ))))
                                    (list (region-x r)
                                          (region-y r)))
                                
                                  ))))
           )
         `(north west south east)
         (iota 4)
         (map make-card-removing-proc
              (list  
               random-choose-card
               dumbest-choose-card
               random-choose-card
               dumbest-choose-card))
         
         ))

  (define *declarer* (assq 'east *player-alist*))
  (set-player-choose-card!! (cdr *declarer*) (make-card-removing-proc interactively-choose-card))
  
  (for-each 
   (lambda (p)
     (send *t* add-region (player-region p)))
   (map cdr *player-alist*))

  (define middle-region
    (let ((cx (/ (send *t* table-width) 2)) ;; center of table
          (cy (/ (send *t* table-width) 2)))
      (make-region (- cx (/ *cw* 2))
                   (- cy (/ *ch* 2))
                   *cw*
                   *ch*
                   #f #f)))

  (send *t* add-region middle-region)
  
  ;; for debugging
  (define (ace-high v)
    (case v
      ((1) 14)
      (else v)))

  (define (card->value c)
    (+ (* 13
          (sub1 (send c get-suit-id)))
       (- (ace-high (send c get-value)) 2)))

  ;; useful only for displaying a hand in this suit order: spades,
  ;; hearts, clubs, diamonds.  This order keeps the hearts and
  ;; diamonds separate, since if they're adjacent there's a risk that
  ;; those with poor vision will think the hearts are diamonds, or
  ;; vice versa, becuase they're both red.
  (define (card->alternate-colors-value c)
    (+ (* 13
          (sub1 (case (send c get-suit-id)
                  ((1) 2)
                  ((2) 1)
                  ((3) 3)
                  ((4) 4))))
       (- (ace-high (send c get-value)) 2)))

  (send *t* set-single-click-action
        (lambda (c)
          (printf "~A~%" 
                  (if (send c face-down?)
                      "Like I'm really gonna show you that card."
                    (format "The ~A of ~A"
                            (let ((v (ace-high (send c get-value))))
                              (case v
                                ;; it's a shame that MzScheme's `format' doesn't
                                ;; support the ~R control.
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
                                ((13) 'king)
                                ((14) 'ace)))
                            (send c get-suit )
                            ))
                  )))

  (random-seed 0)
  (let ()
    (define menu-bar (instantiate menu-bar% () (parent *t*)))

    (define main-menu  (instantiate menu% () 
                         (label "&Stuff" )
                         (parent menu-bar)))
    
    (define deal
      (let ((shuffles 0)
            (shuffle-message (instantiate message% ()
                               (label "Not shuffled")
                               (parent *t*))))
        (lambda ()
          (send *t* remove-cards *d*)
          (for-each (lambda (c) (send c face-down)) *d*)

          ;; deal the cards.
          (set! *d* (shuffle-list *d* 1))
          (set! shuffles (add1 shuffles))
          (let loop ((players *player-alist*)
                     (cards-to-deal *d*))

            (define (first-n l n)
              (let loop ([l l][n n])
                (if (or (null? l)
                        (zero? n))
                    null
                  (cons (car l) (loop (cdr l) (sub1 n))))))

            (when (not (null? cards-to-deal))
          
              (let* ((p (cdar players))
                     (destination (player-region p)))
            
                (set-player-cards! p (first-n cards-to-deal 13))

                (let ((hand (player-cards p)))
                  (send *t* add-cards-to-region 
                        hand
                        (player-region p))

                  (when (eq? (car *declarer*) (caar players))
                    (send *t* cards-face-up hand))
                  
                  (for-each
                   (lambda (c)
                     (send c home-region destination)
                     (send c user-can-flip #f))
                   hand))
                
                (loop (cdr players)
                      (list-tail cards-to-deal 13)))))

          (send shuffle-message set-label (format "~A shuffle(s)" shuffles))

          )))

    (define (sort-declarers-hand)
      ;; sort the cards in the visible hand.
      (let* ((declarer (cdr *declarer*))
             (cards (player-cards declarer))
             (region (player-region declarer)))
            
        (let loop ((cards-to-relocate (mergesort cards (lambda (c1 c2)
                                                         (> (card->alternate-colors-value c1)
                                                            (card->alternate-colors-value c2)))))
                   (cards-located 0))
          (when (not (null? cards-to-relocate))
            (let ((c (car cards-to-relocate)))
              (send/apply *t* move-card c
                          ((player-card-home-location-proc declarer) cards-located))
              
              (send *t* card-to-front c)
              (loop (cdr cards-to-relocate)
                    (add1 cards-located)))))))

    (define (pretend-to-play)
      ;; TODO: I have this nagging feeling that I shouldn't be using a
      ;; loop here, and instead should use some contraption involving
      ;; callbacks which call themselves.  But that sounds ugly.  Wish
      ;; I knew more about GUI programming in general.
      
      ;; One possible advantage of not using loops: if all my code is
      ;; callbacks, and they all complete reasonably quickly (as
      ;; opposed to looping), then the DrScheme window will not say
      ;; "running" for very long, and thus I can futz in the REPL
      ;; pretty much whenever I want (which is, at least, handy for
      ;; debugging).
      (let next-trick ()
        (define (play-one-trick)
          (let next-player ((the-trick '())
                            (cards-this-trick 0))
            (when (< cards-this-trick 4)
              (let* ((p (cdr (list-ref *player-alist* cards-this-trick)))
                     (c ((player-choose-card! p) p)))
                ;; play the first card in the hand, because we're dumb.
                (send *t* card-face-up c)
                (send *t* card-to-front c) ;TODO -- use `stack-cards' instead
                (let ((region-center-x (+ (region-x (player-region p)) (/ (region-w (player-region p)) 2)))
                      (region-center-y (+ (region-y (player-region p)) (/ (region-h (player-region p)) 2))))
                  (let ()
                   (send *t* move-card-center c 
                         (+ (/ *ch* 2) (region-x middle-region) (* (- region-center-x (region-x middle-region)) 1/8))
                         (+ (/ *ch* 2) (region-y middle-region) (* (- region-center-y (region-y middle-region)) 1/8)))))
                (next-player (cons c the-trick)
                             (add1 cards-this-trick))))

            ;; TODO: find out why the program pauses for a long time
            ;; *after* the trick vanishes from the table.  Both
            ;; (sleep) and (send *t* pause) seem to suffer from this.
            ;; For what it's worth, the `pause' method differs from
            ;; sleep in that it does its sleeping in a separate thread
            ;; (and uses `yield' to wait for that sleep to finish) ,
            ;; and hence presumably doesn't block *this* thread.
            (send *t* pause .1)

            (send *t* remove-cards the-trick)
            (for-each (lambda (c) (send c face-down)) the-trick)))
        (let ((cards (player-cards (cdar *player-alist*))))
          (when (not (null? cards))
            (play-one-trick)
            (next-trick)
            ))
        )
      (printf "OK, we're done with that hand.~%"))
          
    (send menu-bar enable #t)

    (instantiate menu-item% ()
      (label "&Exit")
      (parent main-menu)
      (callback (lambda (item event)
                  (exit))))

    (instantiate menu-item% ()
      (label "&Deal")
      (parent main-menu)
      (callback (let ((sort-menu-item
                       (instantiate menu-item% ()
                         (label "&Sort declarer's hand")
                         (parent main-menu)
                         (callback (lambda (item event)
                                     (sort-declarers-hand))))))
                  (send sort-menu-item enable #f)
                  (lambda (item event)
                    (deal)
                    (send sort-menu-item enable #t)
                    (pretend-to-play)
                    (send sort-menu-item enable #f)))))

    ;; Move the table so that it's all visible, and not blocking the
    ;; DrScheme window.
    (send *t* move 400 0)
            
    (send *t* show #t)))
