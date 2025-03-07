#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mred -qu "$0" ${1+"$@"}
|#

;; TODO:

;; find a better way to chatter than printf.  Perhaps make a scrolling
;; console that captures standard out.  (How nerdly.)

;; figure out how to draw cards oriented horizontally.  I posted to
;; plt-scheme@list.cs.brown.edu on May 10, 2003 asking this.  The answer:
;; hack the source to the card library.

;; figure out how to deliver a stand-alone executable (see the dox for
;; `write-image-to-file' in the mzscheme manual).  The executable
;; created by `mzc --gui-exe bridge bridge.ss' is *almost* standalone,
;; but it loads something (probably bitmaps) from games/cards at
;; startup.

(module bridge mzscheme
  (require (lib "cards.ss" "games" "cards"))
  (require (all-except (lib "classes.ss" "games" "cards") pasteboard%))
  (require (lib "class.ss"))
  (require (lib "mred.ss" "mred"))
  (require (only (lib "list.ss") sort remove))
  (require "rotate.ss")
  (require "call.ss")
  (require "interactive-call.ss")
  (require "auction.ss")
  (require "misc.ss")

  ;; this rename looks dumb -- hell, it *is* dumb -- but it's the only
  ;; way I can think of to avoid a name collision -- apparently 1.ss
  ;; and list.ss both define some of the same names (such as "second",
  ;; "third", etc.)  Newer versions of mzscheme allow the keyword
  ;; "only" in the "require" form.
  (require (rename (lib "1.ss" "srfi") iota iota))

  (define *d* (make-deck))
  (for-each (lambda (c) (send c user-can-flip #f)) *d*)
  (define *cw* (send (car *d*) card-width))
  (define *ch* (send (car *d*) card-height))

  (define-syntax quickly
    (syntax-rules ()
      ((_ body ...)
       (dynamic-wind
        (lambda () (send *t* begin-card-sequence))
        (lambda () body ... )
        (lambda () (send *t* end-card-sequence))))))

  (define my-table%
    (class table%

      (inherit move-card)
      (public move-card-center)

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

  (define dumbest-choose-card car)

  (define (random-choose-card hand)
    (list-ref hand (random (length hand))))

  (define interactively-choose-card
    (let ((choice #f))
      (lambda (hand)
        (let ((sem (make-semaphore)))

          ;; Doesn't this clobber any existing double-click-action?  Isn't
          ;; that a Bad Thing?
          (send
           *t*
           set-double-click-action
           (lambda (card)
             (printf "Double-clicked ~s~%" card)
             (if (member card hand)
                 (set! choice card)
               (printf
                (if
                    ;; this isn't quite the right test -- I
                    ;; really want to say something like "if
                    ;; this card doesn't belong to the human".
                    ;; The human's cards include their own
                    ;; hand, of course, but also, if they're
                    ;; the declarer, the dummy.

                    (send card face-down?)
                    "You gotta click your own cards.~%"
                  "It's not that player's turn.~%")))
             (semaphore-post sem)))
          (printf "OK, double-click a card.~%")
          (yield sem)
          (unless choice
            (error "Looks like I don't understand semaphores."))
          choice))))

  (define (make-card-removing-proc choice-proc)
    (lambda (p)
      (let ((choice (choice-proc (player-cards p))))
        (set-player-cards! p (remove choice (player-cards p)))
        choice)))

  (define *direction/player-alist*
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

                                ;; TODO -- always sort horizontal
                                ;; hands from left to right
                                (lambda (index)
                                  (let ((r (rotate-region
                                            (make-region
                                             (+ (region-x north-region) (* (/ (region-w north-region) 14)

                                                                           ;; If it's the interactive hand,
                                                                           ;; put the high cards on the left.

                                                                           (if (= 2 quarter-turns)
                                                                               (- 12 index)
                                                                             index)

                                                                           ))
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
         *compass-directions*
         `(0 3 2 1)
         (map make-card-removing-proc
              (list
               random-choose-card
               dumbest-choose-card
               random-choose-card
               dumbest-choose-card))

         ))

  (define *interactive-hand* (assq 'south *direction/player-alist*))
  (set-player-choose-card!! (cdr *interactive-hand*) (make-card-removing-proc interactively-choose-card))

  (define *dummy* (assq 'west *direction/player-alist*))

  (for-each
   (lambda (p)
     (send *t* add-region (player-region p)))
   (map cdr *direction/player-alist*))

  (define middle-region
    (let ((cx (/ (send *t* table-width) 2)) ;; center of table
          (cy (/ (send *t* table-width) 2)))
      (make-region (- cx (/ *cw* 2))
                   (- cy (/ *ch* 2))
                   *cw*
                   *ch*
                   #f #f)))

  (send *t* add-region middle-region)

  (random-seed 0)

  ;; TODO -- see if perhaps we could use a toolbar instead of a menu
  ;; bar.
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
        (let loop ((players *direction/player-alist*)
                   (cards-to-deal *d*))

          (when (not (null? cards-to-deal))

            (let* ((p (cdar players))
                   (destination (player-region p)))

              (set-player-cards! p (first-n cards-to-deal (/ (length *d*)
                                                             (length *direction/player-alist*))))

              (let ((hand (player-cards p)))
                (send *t* add-cards-to-region
                      hand
                      (player-region p))

                (when (eq? (car *interactive-hand*) (caar players))
                  (send *t* cards-face-up hand))

                (for-each
                 (lambda (c)
                   (send c home-region destination))

                 hand))

              (loop (cdr players)
                    (list-tail cards-to-deal (length (player-cards p)))))))

        (send shuffle-message set-label (format "~A shuffle(s)" shuffles))

        )))

  (define (sort-hand visible-hand)
    (let* ((cards (player-cards visible-hand))
           (region (player-region visible-hand)))

      ;; useful only for displaying a hand in this suit order: spades,
      ;; hearts, clubs, diamonds.  This order keeps the hearts and
      ;; diamonds separate, since if they're adjacent there's a risk that
      ;; those with poor vision will think the hearts are diamonds, or
      ;; vice versa, becuase they're both red.
      (define (card->alternate-colors-value c)
        (define (ace-high v)
          (case v
            ((1) 14)
            (else v)))
        (+ (* 13
              (sub1 (case (send c get-suit-id)
                      ((1) 2)
                      ((2) 1)
                      ((3) 3)
                      ((4) 4))))
           (- (ace-high (send c get-value)) 2)))

      (let loop ((cards-to-relocate (sort cards ;; decreasing left-to-right
                                          (lambda (c1 c2)
                                            (> (card->alternate-colors-value c1)
                                               (card->alternate-colors-value c2)))))
                 (cards-located 0))
        (when (not (null? cards-to-relocate))
          (let ((c (car cards-to-relocate)))
            (send/apply *t* move-card c
                        ((player-card-home-location-proc visible-hand)
                         cards-located ))

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
    (let next-trick ((tricks-played 0))
      (define (play-one-trick)

        (define (ordinal hand-pair)
          (- (length *direction/player-alist*)
             (length (member (car hand-pair) (map car *direction/player-alist*)))))

        (define dummy-offset (ordinal *dummy*))
        (let next-player ((the-trick '())
                          (cards-this-trick 0))
          (when (and (zero? tricks-played)
                     (= 1 cards-this-trick))
            (quickly
             (send *t* cards-face-up (player-cards (cdr *dummy*)))
             (sort-hand (cdr *dummy*)))

            (when (= 2 (modulo (- (ordinal *interactive-hand*)
                                  (ordinal *dummy*))
                               4))
              (printf "Looks like we're the declarer, so we get to play both hands.~%")
              (set-player-choose-card!! (cdr *dummy*) (player-choose-card! (cdr *interactive-hand*)))))

          (when (< cards-this-trick 4)
            (let* ((p (cdr (list-ref *direction/player-alist* (modulo
                                                     (+ cards-this-trick (- dummy-offset 1))
                                                     4))))
                   (c ((player-choose-card! p) p)))
              (send *t* card-face-up c)
              (send *t* card-to-front c) ;; maybe use `stack-cards' instead
              (let ((region-center-x (+ (region-x (player-region p)) (/ (region-w (player-region p)) 2)))
                    (region-center-y (+ (region-y (player-region p)) (/ (region-h (player-region p)) 2))))
                ;; move the card almost, but not quite, to the
                ;; center of the table.  That way none of the cards
                ;; of the trick are entirely obscured by the others.
                (send *t* move-card-center c
                      (+ (/ *ch* 2) (region-x middle-region) (* (- region-center-x (region-x middle-region)) 1/8))
                      (+ (/ *ch* 2) (region-y middle-region) (* (- region-center-y (region-y middle-region)) 1/8))))
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
          (for-each (lambda (c) (send c face-down)) the-trick)
          ))
      (when (not (null? (player-cards (cdar *direction/player-alist*))))
        (play-one-trick)
        (next-trick (add1 tricks-played))
        )
      )
    (printf "OK, we're done with that hand.~%"))

  (send menu-bar enable #t)

  ;; TODO -- figure out how to make this not be global.  This may be
  ;; one of those cases where syntax-rules' lack of variable capture
  ;; is a Bad Thing, in which case I should simply re-write it with
  ;; defmacro.
  (define *menu-items* '())
  (define-syntax menite
    (syntax-rules ()
      ((_ lbl callback-body ...)
       (let ((mi (instantiate menu-item% ()
                   (label lbl)
                   (parent main-menu)
                   (callback (lambda (item event)
                               callback-body ...)))))
         (send mi enable #f)
         (set! *menu-items* (cons mi *menu-items*))
         mi))))

  (define exit-menu-item (menite "&Exit" (exit)))

  (define sort-menu-item
    (menite
     "&Sort hand"
     (sort-hand (cdr *interactive-hand*))))

  (define deal-menu-item
    (menite
     "&Deal"
     (deal)
     (for-each (lambda (m) (send m enable #t))
               (list
                deal-menu-item
                sort-menu-item
                auction-menu-item))))

  (send exit-menu-item enable #t)
  (make-bbox-window *t*)
  (define auction-menu-item
    (menite
     "&Auction"

     (let ((the-auction (make-auction (caar *direction/player-alist*))))
       (reset-buttons-for-new-auction)
       (let loop ()
         (let* ((p (whose-turn the-auction))
                (player-is-interactive? (eq? p
                                             (car
                                              *interactive-hand*))))
           (define (dumbly-get-call auction)
             (printf "~A dumbly passes.~%" p)
             'pass)
           (let ((c (if player-is-interactive?
                        (interactively-get-call the-auction (format "~A" p))
                      (dumbly-get-call the-auction))))
             (note-call the-auction c))

           (if (contract-settled? the-auction)
               (printf "Final bid was ~A~%" (call->string (last-bid the-auction)))
             (loop)))))
     (send auction-menu-item enable #f)
     (pretend-to-play)))

  (send deal-menu-item enable #t)

  ;; Move the table so that it's all visible, and not blocking the
  ;; DrScheme window.
  (send *t* move 400 0)

  (send *t* show #t))
