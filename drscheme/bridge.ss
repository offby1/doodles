;; TODO in general -- find a better way to chatter than printf.
;; Perhaps make a scrolling console that captures standard out.

(module bridge mzscheme
  (require (lib "cards.ss" "games" "cards"))
  (require (all-except (lib "classes.ss" "games" "cards") pasteboard%))
  (require (lib "class.ss"))
  (require (lib "mred.ss" "mred"))
  (require (lib "list.ss"))
  (require "fys.ss")
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

  (define *t* (make-object my-table% "snowball" 10 10))
  (send *t* set-button-action 'left   'drag-raise/one)
  (send *t* set-button-action 'middle 'drag-raise/one)
  (send *t* set-button-action 'right  'drag-raise/one)

  (define *region-length* (- (send *t* table-width) (* 2 *ch*)))

  (define-struct player (region cards choose-card!))

  (define (dumbest-choose-card p)
    (let ((choice (car (player-cards p))))
      (set-player-cards! p (cdr (player-cards p)))
      choice))

  (define (random-choose-card p)
    (let* ((hand (player-cards p))
           (choice (list-ref hand (random (length hand)))))
      (set-player-cards! p (remove choice hand))
      choice))

  (define interactively-choose-card
    (let ((choice #f))
      (lambda (p)
        (let ((sem (make-semaphore)))
      
          ;; Doesn't this clobber any existing double-click-action?  Isn't
          ;; that a Bad Thing?
          (send *t* set-double-click-action 
                (lambda (card)
                  (if (member card (player-cards p))
                      (begin
                        (set! choice card)
                        (semaphore-post sem))
                    (begin
                      (bell)
                      (printf "You gotta click your own cards.")))
                  ))
          (yield sem)
          (unless choice
            (error "Looks like I don't understand semaphores."))
          choice))))

  (define *player-alist*
    (map (lambda (pos coords cards choice-func)
           (cons pos
                 (make-player (apply make-region (append coords (list (symbol->string pos) #f))) 
                              cards (lambda (p)
                                      (printf "Waiting for player ~A~%" pos)
                                      (choice-func p)))))
         `(south west north east)
         `( 
           ;;left x                           top y                           width                   height

;;; south
           (,*ch*                             ,(+ *ch* *region-length*)       ,*region-length*        ,*ch*           ) 

;;; west
           (0                                 ,*ch*                           ,*ch*                   ,*region-length*) 

;;; north
           (,*ch*                             0                               ,*region-length*        ,*ch*           )

;;; east
           (,(+ *ch* *region-length*)         ,*ch*                           ,*ch*                   ,*region-length*)
           )
         `(#f #f #f #f)
         `(,interactively-choose-card
           ,dumbest-choose-card
           ,random-choose-card
           ,dumbest-choose-card)
         ))
  
  (for-each 
   (lambda (p)
     (send *t* add-region (player-region p)))
   (map cdr *player-alist*))

  (define middle-region
    (let ((cx (/ (send *t* table-width ) 2)) ;; center of table
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

  (send *t* set-single-click-action
        (lambda (c)
          (printf "~A~%" 
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
                          (send c get-suit )))))

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
          (send *t* cards-face-down *d*)

          ;; deal the cards.
          (set! *d* (shuffle-list *d* 1))
          (set! shuffles (add1 shuffles))
          (let loop ((players *player-alist*)
                     (d *d*))

            (define (first-n l n)
              (let loop ([l l][n n])
                (if (or (null? l)
                        (zero? n))
                    null
                  (cons (car l) (loop (cdr l) (sub1 n))))))

            (when (not (null? d))
          
              (let* ((p (cdar players))
                     (destination (player-region p)))
            
                (set-player-cards! p (first-n d 13))

                (let ((hand (player-cards p)))
                  (send *t* add-cards-to-region 
                        hand
                        (player-region p))

                  (when (eq? 'south (caar players))
                    (send *t* cards-face-up hand))
                  
                  (for-each
                   (lambda (c)
                     (send c home-region destination)
                     (send c user-can-flip #f))
                   hand))
                
                (loop (cdr players)
                      (list-tail d 13)))))

          (send shuffle-message set-label (format "~A shuffle(s)" shuffles))

          )))

    (define (sort)
      (if #t
          (printf "Not sorting hand 'cuz that's slow.~%")
        ;; sort the cards in the visible hand.
        (let ((south (cdr (assq 'south *player-alist*))))
            
          (let loop ((hand (mergesort (player-cards south) (lambda (c1 c2)
                                                             (> (card->value c1)
                                                                (card->value c2)))))
                     (x (region-x (player-region south))))
            (when (not (null? hand))
              (let ((c (car hand)))
                (send *t* move-card c
                      x
                      (region-y (player-region south)))
              
                (send *t* card-to-front c)
                (loop (cdr hand)
                      (+ x (/ *region-length* 13)))))))))

    (define (pretend-to-play)
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
                  (quickly
                   (send *t* move-card-center c 
                         (+ (/ *ch* 2) (region-x middle-region) (* (- region-center-x (region-x middle-region)) 1/8))
                         (+ (/ *ch* 2) (region-y middle-region) (* (- region-center-y (region-y middle-region)) 1/8)))))
                (next-player (cons c the-trick)
                             (add1 cards-this-trick))))

            ;; TODO: find out why the program pauses for a long time
            ;; *after* the trick vanishes from the table.
            (sleep/yield .1)

            (send *t* remove-cards the-trick)
            (for-each (lambda (c) (send c face-down)) the-trick)))
        (when (not (null? (player-cards (cdar *player-alist*))))
          (play-one-trick)
          (next-trick)
          )))
          
    (send menu-bar enable #t)

    (instantiate menu-item% ()
      (label "&Exit")
      (parent main-menu)
      (callback (lambda (item event)
                  (exit))))
    (instantiate menu-item% ()
      (label "&Deal")
      (parent main-menu)
      (callback (lambda (item event)
                  (deal))))

    ;; Move the table so that it's all visible, and not blocking the
    ;; DrScheme window.
    (send *t* move 400 0)
            
    (send *t* show #t)
    (let ()
      (define one-hand
        (lambda ()
          (deal)
          (sort)
          (pretend-to-play)
          )
        )
      ;;(let loop () (one-hand) (loop))
      (instantiate timer% () (notify-callback one-hand) (interval 1000))
      )))

