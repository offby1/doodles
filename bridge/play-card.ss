(module play-card mzscheme
  (require (lib "list.ss" "srfi" "1")
           (lib "trace.ss")
           "auction.ss"
           "deck.ss"
           "hand.ss")
  (print-struct #t)
  (provide play-card)
  (define (play-card vulnerability auction-so-far dealer my-hand plays-so-far)
    (play-ref
     (best-course-of-play-from-prefix my-hand plays-so-far)
     (play-length plays-so-far)))
  (trace play-card)
  (define-syntax append-to-history!
    (syntax-rules ()
      ((_ history card)
       (begin
         (set! history (append history (list card)))
         history))))
  
  (define (best-course-of-play-from-prefix my-hand plays-so-far)
    (if (= (hand-length my-hand) 1)
        (list (hand-ref my-hand 0))
      (first (map
              (lambda (c)
                (best-course-of-play-from-prefix (without-card c my-hand)
                                                 (append-to-history! plays-so-far c)))
              (legal-plays my-hand plays-so-far)))))

  (define (play-ref history K)
    (list-ref history K))
  (define play-length length)

  (define (legal-plays my-hand plays-so-far)
    (case (hand-length my-hand)
      ((0 1) (error "This isn't supposed to happen."))
      (else
       (list (hand-ref my-hand 0)
             (hand-ref my-hand 1))))
    )
  ;(trace legal-plays)
  (define (history-add history card)
    (cons card history))
  (define (make-empty-hand-history)
    '())

  ;; a little exercise
  (let* ((a (make-auction 'east))
         (all-hands (shuffled-deck))
         (my-hand (holding all-hands 'south)))
    (auction-add! a 'pass)
    (auction-add! a 'pass)
    (auction-add! a '(1 diamond))
    (auction-add! a 'pass)
    (auction-add! a 'pass)
    (auction-add! a 'pass)
    (let loop ((hand-history (make-empty-hand-history)))
      (printf "History so far: ~s~n" hand-history)
      (if (= (length hand-history)
             *deck-size*)
          (printf "OK, we're done~n")   ;TODO: compute score
        (let ((c (play-card 'dunno a 'east my-hand hand-history)))
          (printf "~a~n" (card->string c))
          (loop (history-add hand-history c))))))
  )
