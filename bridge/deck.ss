(module deck mzscheme
  (require (lib "list.ss" "srfi" "1")
           (lib "13.ss" "srfi")
           "constants.ss")
  (define *deck-size* 52)
  (define *ranks* (iota 13 2))
  
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
  
  (define (my-make-deck)
    (make-deck (list->vector (iota *deck-size*))))

  (define (deck->string d)
    (string-join
     (map card->string (vector->list (deck-v d)))
     ", "))

  (define d (my-make-deck))
  (printf "Hey look! ~a~n" (deck->string d))
  )