(define (all-splits deck)

  (define hands (make-vector 4))

  (vector-set! hands 0 (hand 0 deck))
  (vector-set! hands 1 (hand 1 deck))
  (vector-set! hands 2 (hand 2 deck))
  (vector-set! hands 3 (hand 3 deck))

  (let ()
    (define (splits suit)
      (define (cards-of-suit suit hand)
        (let loop ((cards-examined 0)
                   (sum 0))
          (if (< cards-examined (vector-length hand))
              (loop (+ 1 cards-examined)
                    (+ sum 
                       (if (= suit (remainder (vector-ref hand cards-examined) 4))
                           1
                         0)))    
            sum)))
  
      (list
       (list (cards-of-suit suit (vector-ref hands 0))
             (cards-of-suit suit (vector-ref hands 2)))
       (list (cards-of-suit suit (vector-ref hands 1))
             (cards-of-suit suit (vector-ref hands 3)))))
    (append
     (splits 0)
     (splits 1)
     (splits 2)
     (splits 3))))

(define (lopsidedness-of-most-unbalanced-hand deck)
  (apply max (map (lambda (p) (abs (apply - p))) (all-splits deck))))

;; Deal 100 deals, and report the lopsidedness of the most unbalanced
;; of those hands.
(apply max (repeat 100 (lambda (dummy) (lopsidedness-of-most-unbalanced-hand (new-deck)))))
