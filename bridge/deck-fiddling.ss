(module deck-fiddling mzscheme
  (require "constants.ss"
           "hand.ss"
           "deck.ss"
           "card.ss"
           (lib "list.ss" "srfi" "1")
           (rename (lib "compat.ss") sort sort))
  

  ;; hand -> four-element list
  (define (shape h)
    (set! h (hand->list h))
    (map (lambda (s)
           (length (filter (lambda (c) (eq? s (card-suit c))) h))
           )
         *suits*)
    )
    
  (define (hand->string h)
    (map card->short-string (sort card< (hand->list h))))
  
  (define (summarize-hands d)
    (map (lambda (s)
           (let ((h (holding d s)))
             (format "~a holds ~a; shape is ~a~n" s (hand->string h) (shape h))))
         *seats*))
  
  (let loop ((d (shuffled-deck))
             (passes 0))
    (when (< passes 3)
      (display (summarize-hands d))
      (newline)
      (loop (shuffled-deck) (add1 passes))
      )
    ))