(module deal-randomly mzscheme
  (require (lib "cards.ss" "games" "cards"))
  (require (all-except (lib "classes.ss" "games" "cards") pasteboard%))
  (require (lib "class.ss"))
  (require (lib "mred.ss" "mred"))
  (require (lib "list.ss"))
  (require "fys.ss")
  (define *d* (let ((d (shuffle-list (make-deck) 1)))
                (for-each (lambda (c)
                           (send c face-up))
                         d)
                d))

  (define *cw* (send (car *d*) card-width))
  (define *ch* (send (car *d*) card-height))

  ;; TODO: think of a more elegant way to do this
  (define *quit-flag* #f)
  
  (define my-table%
    (class table% 
      (rename (o-s-c on-subwindow-char))
      (override on-subwindow-char)
      (define on-subwindow-char 
        (lambda (receiver event) 
          (when (eq? 'escape (send event get-key-code))
            (set! *quit-flag* #t))
          
          (o-s-c receiver event)))
      (super-instantiate ())))

  (define *t* (make-object my-table% "snowball" 10 10))
  (send *t* set-button-action 'left   'drag-raise/one)
  (send *t* set-button-action 'middle 'drag-raise/one)
  (send *t* set-button-action 'right  'drag-raise/one)

  (define (card->value c)
    (+ (* 13
          (sub1 (send c get-suit-id)))
       (* 1
          (sub1 (send c get-value)))))

  (define *region-length* (- (send *t* table-width) (* 2 *ch*)))
  (define *player-regions*
    (map
     (lambda (direction coords)
       (apply make-region (append coords (list (symbol->string direction)
                                               #f))))
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
       )))

  (for-each
   (lambda (r)
     (send *t* add-region r))
   *player-regions*)

  (define (card->initial-position c)
    (list
     (* (- *region-length*
           (/ *cw* 2)) 
        (/
         (card->value c) 52))
     (- (send *t* table-height)
        *ch*)))

  (let loop ((hands-dealt 0)
             (d *d*))
    (define (first-n l n)
      (let* ((copy (apply list l))
             (tail (list-tail copy n)))
        (when (not (null? tail))
          (set-cdr! tail '()))
        copy))

    (when (not (null? d))
      
      (let ((destination (list-ref *player-regions* hands-dealt))
            (hand (first-n d 13)))

        (for-each
         (lambda (c) (send c home-region destination))
         hand)

        (send *t* add-cards-to-region 
              hand
              destination))
                                                                  
      (loop (add1 hands-dealt)
            (list-tail d 13))))
  
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
    
    (send menu-bar enable #t)

    (instantiate menu-item% ()
                 (label "&Exit")
                 (parent main-menu)
                 (callback (lambda (item event)
                             (exit))))
    (send *t* show #t)))
