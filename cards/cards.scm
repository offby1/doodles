;; Stuff to examine statistical properties of a bridge hand.  For
;; example, if I deal 1000 hands, what's the longest suit that I can
;; expect to appear?

(define (new-deck) 
  
  ;; An unshuffled deck.  Each element is an integer representing a
  ;; card.  The card's suit is the remainder of the integer when
  ;; divided by four.  The rank would be the integer quotient when
  ;; divided by four (except this code doesn't use the rank).
  (let ((virgin (make-vector 52)))

    ;; Return a list that has the same elements as L, but in random
    ;; order.
    (define (shuffle-list l)
      (map
       car
       (sort

        ;; Make a new list whose elements are conses; each car is the
        ;; corresponding element of the input list, and each cdr is a
        ;; random number.
        (map
         (lambda (entry)
           (cons entry (random most-positive-fixnum)))
         l)

        (lambda (entry1 entry2)
          (< (cdr entry1)
             (cdr entry2))))))

    (define (shuffle-vector v)
      (list->vector
       (shuffle-list
        (vector->list v))))

    (let loop ((slots-initialized 0))
      (if (< slots-initialized (vector-length virgin))
          (begin
            (vector-set! virgin slots-initialized slots-initialized)
            (loop (+ 1 slots-initialized)))
        (shuffle-vector virgin)))))

(define (hand n deck)
  (let ((return (make-vector 13)))
    (let loop ((slots-initialized 0))
      (if (< slots-initialized (vector-length return))
          (begin
            (vector-set! 
             return 
             slots-initialized 
             (vector-ref deck (+ slots-initialized (* 13 n))))
            (loop (+ 1 slots-initialized)))
        return))))

(define (tally item alist)
  (let ((cell (assq item alist)))
    (if cell
        (begin
          (set-cdr! cell (+ 1 (cdr cell)))
          alist)
      (cons (cons item 1)
            alist))))

(define (longest-suit hand)
  (define (suit number) (remainder number 4))

  (let ((return '()))
    (for-each (lambda (card)
                (set! return (tally (suit card) return)))
              (vector->list hand))
    (apply max (map cdr return))))

(define (repeat n thing)
  (if (not (and (exact? n)
                (integer? n)
                (not (negative? n))))
      (error "N must be an exact non-negative integer, but is" n))

  (let loop ((n n)
             (result '()))
    (if (zero? n)
        (reverse result)
      (loop (- n 1)
            (cons (thing n) result)))))

;; Deal 100 deals, and report the length of the longest suit in the
;; most unbalanced hand.
(apply max (repeat 100 (lambda (dummy) (longest-suit (hand 0 (new-deck))))))
