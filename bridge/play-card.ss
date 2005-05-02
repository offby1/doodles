(module play-card mzscheme
  (require (lib "list.ss" "srfi" "1")
           "auction.ss")
  (provide play-card)
  (define (play-card vulnerability auction-so-far dealer my-hand plays-so-far)
    (play-ref
     (best-course-of-play-from-prefix my-hand plays-so-far)
     (play-length plays-so-far)))

  (define (best-course-of-play-from-prefix my-hand plays-so-far)
    (first (legal-plays my-hand plays-so-far)))
  (define play-ref list-ref)
  (define play-length length)
  (define (legal-plays my-hand plays-so-far)
    (car my-hand))

  ;; a little exercise
  (let* ((a (make-auction 'east))
         (all-hands (deal))
         (my-hand (holding all-hands 'south)))
    (auction-add! a 'pass)
    (auction-add! a 'pass)
    (auction-add! a '(1 diamond))
    (auction-add! a 'pass)
    (auction-add! a 'pass)
    (auction-add! a 'pass)
    (let loop ((hand-history (make-empty-hand-history)))
      (if (hand-complete? hand-history)
          (printf "OK, we're done~n")   ;TODO: compute score
        (let ((c (play-card 'dunno a 'east my-hand hand-history)))
          (history-add! hand-history c)
          (printf "~a~n" (card->string c))
          (loop hand-history)))))
  )