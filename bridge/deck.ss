(module deck mzscheme
  (provide
   shuffled-deck
   holding
   deck-length
   *deck-size*
   deck-ref
   card->string)
  (require (lib "list.ss" "srfi" "1")
           (lib "13.ss" "srfi")
           (rename (lib "43.ss" "srfi") vector-copy vector-copy)
           "constants.ss"
           "hand.ss")

  (define-struct deck (v))
  
  (define (rank-string r)
    (case r
      ((11) "jack")
      ((12) "queen")
      ((13) "king")
      ((14) "ace")
      (else
       (format "~a" r))))
  
  (define (card->string thing)
    (let* ((rank# (+ (car *ranks*) (modulo   thing (length *ranks*))))
           (suit# (quotient thing (length *ranks*)))
           (suit-string (list-ref *suits* suit#)))
      (format "the ~a of ~a" (rank-string rank#) suit-string)))
  
  (define (shuffled-deck)
    (define (fisher-yates-shuffle v)
      (define (swap! i1 i2)
        (let ((tmp (vector-ref v i1)))
          (vector-set! v i1 (vector-ref v i2))
          (vector-set! v i2 tmp)))
      (let ((l (vector-length v)))
        (do ((top-index (sub1 l) (sub1 top-index)))
            ((zero? top-index) v)
          (let ((bottom-index (random top-index)))
            (swap! bottom-index top-index)))))
    (make-deck (fisher-yates-shuffle (list->vector (iota *deck-size*)))))

  (define (deck->string d)
    (string-join
     (map card->string (vector->list (deck-v d)))
     ", "))

  (define (deck-length d)
    (vector-length (deck-v d)))

  (define (deck-ref d k)
    (vector-ref (deck-v d) k))
  
  (define (holding deck seat)
    (let ((cards-per-hand (/ (deck-length deck)
                             (length *seats*))))
      (make-hand
       (vector-copy
        (deck-v deck)
        (* cards-per-hand (seat->number seat))
        (* cards-per-hand (add1 (seat->number seat)))))
      ))
  )
