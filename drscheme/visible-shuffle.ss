(module visible-shuffle mzscheme
  (require (lib "cards.ss" "games" "cards"))
  (require (lib "class.ss"))
  (require (lib "mred.ss" "mred"))
  (require "fys.ss")
  (define *d* (make-deck))
  (define *cw* (send (car *d*) card-width))
  (define *ch* (send (car *d*) card-height))
  (define *tw* 13)
  (define *th* 4)
  (define (1- x)
    (- x 1))
  (define *t* (make-table "snowball" *tw* *th*))
  (define *r* (make-region 0 0
                         (- *tw* 1)
                         (- *th* 1)
                         "cards go here" #f))
  
  (define (card->value c)
    (+ (* 13
          (1- (send c get-suit-id)))
       (* 1
          (1- (send c get-value)))))

  (define (card->initpos c)
    (values
     (* *cw* (1- (send c get-value  )))
     (* *ch* (1- (send c get-suit-id)))))

  (define (card->initpos* c)
    (call-with-values
        (lambda ()
          (card->initpos c))
      cons))

  (define (card->string c)
    (format "The ~A of ~A"
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
            (send c get-suit )))
  
  (define initial-position
    (lambda (index)
      (card->initpos (list-ref *d* index))
      ))

  (send *t* add-cards *d* 0 0 initial-position)
  (send *t* show #t)
  
  ;; for debugging
  (send *t* set-single-click-action
        (lambda (card)
          (printf "~A~%" (card->string card))))
  
  (sleep/yield 1)
  (send *t* flip-cards *d*)

  (let ((location-alist (map (lambda (c) (cons c (card->initpos* c)))
                             *d*)))
    (define (swap-cards c1 c2)
      ;; there's something fishy here.  I shouldn't have to maintain
      ;; the locations of the cards myself, and yet I cannot figure
      ;; out how to get the table to tell me where they are.


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
          (move-and-update c2 loc-pair-1))))
    (sleep/yield 1)
    ;;(swap-cards (car *d*) (cadr *d*))
    
    ;; It would be good to assign this action to a callback which I
    ;; could then install on a menu item named "shuffle".
    (set! *d* (vector->list (fys! (apply vector *d*) swap-cards)))
    (printf "Done.\n")
    ))

