(module play-card mzscheme
  (require (lib "list.ss" "srfi" "1")
           (lib "trace.ss")
           "auction.ss"
           "deck.ss"
           "hand.ss"
           "play-history.ss")
  (print-struct #t)
  (provide play-card)

  (define (play-card vulnerability auction-so-far dealer my-hand plays-so-far)
    (history-ref
     (best-course-of-play-from-prefix my-hand plays-so-far)
     (history-length plays-so-far)))
  ;(trace play-card)

  (define (best-course-of-play-from-prefix my-hand plays-so-far)
    (if (= (hand-length my-hand) 1)
        (history-add plays-so-far (first (hand->list my-hand)))
      (let ((c (first (legal-plays my-hand plays-so-far))))
        (best-course-of-play-from-prefix (without-card c my-hand)
                                         (history-add plays-so-far c)))))
  ;(trace best-course-of-play-from-prefix)

  (define (legal-plays my-hand plays-so-far)
    (case (hand-length my-hand)
      ((0 1) (error "This isn't supposed to happen."))
      (else
       (list (hand-ref my-hand 0)
             (hand-ref my-hand 1))))
    )
  ;(trace legal-plays)


  (random-seed 0)
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
    (let loop ((history (make-empty-history))
               (my-hand my-hand))
      (if (zero? (hand-length my-hand))
          (printf "OK, we're done~n")   ;TODO: compute score
        (let ((c (play-card 'dunno a 'east my-hand history)))
          (printf "~a~n" (card->string c))
          (loop (history-add history c)
                (without-card c my-hand))))))
  )
