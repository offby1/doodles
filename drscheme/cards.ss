(module cards mzscheme
  (require (lib "cards.ss" "games" "cards"))
  (require (lib "class.ss"))
  (require (lib "mred.ss" "mred"))
  (require "fys.ss")
  (define d (make-deck))
  (define cw (send (car d) card-width))
  (define ch (send (car d) card-height))
  (define tw 13)
  (define th 4)
  (define (1- x)
    (- x 1))
  (define t (make-table "snowball" tw th))
  (define r (make-region 0 0
                         (- tw 1)
                         (- th 1)
                         "cards go here" #f))
  
  (define (card->value c)
    (+ (* 13
          (1- (send c get-suit-id)))
       (* 1
          (1- (send c get-value)))))

  (define (card->initpos c)
    (values
     (* cw (1- (send c get-value  )))
     (* ch (1- (send c get-suit-id)))))

  (define (card->string c)
    (format "The ~A of ~A"
            (send c get-value)
            (send c get-suit )))
  
  (define initial-position
    (lambda (index)
      (card->initpos (list-ref d index))
      ))

  (send t add-cards d 0 0 initial-position)
  (let ()
    (define (kids thing)
      (if (is-a? thing area-container<%>)
          (let ((immediate-kids (send thing get-children)))
            (append immediate-kids
                    (map kids immediate-kids)))
        `(thing)))
    (printf "Info about t(~S): ~S~%" t (kids t)))
  (send t show #t)
  
  ;; for debugging
  (send t set-single-click-action
        (lambda (card)
          (printf "~A~%" (card->string card))))
  
  (sleep/yield 1)
  (send t flip-cards d)

  (let ()
    (define (swap-cards c1 c2)
      (let-values (((c1x c1y) (card->initpos c1)))
        (call-with-values
            (lambda ()  (card->initpos c2))
          (lambda (x y) (send t move-card c1 x y)))
        (send t move-card c2 c1x c1y)))

    (sleep/yield 1)
    (map card->value (vector->list (fys! (apply vector d))))
    (swap-cards (car  d)
                (cadr d))))

