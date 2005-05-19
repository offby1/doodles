(module card mzscheme
  (provide
   card->string
   card->short-string
   card-suit
   card<
   make-card-from-number
   card-rank
   (rename my-make-card make-card))
  (require "constants.ss"
           "exceptions.ss")

  (define *num-ranks* (length *ranks*))
  (define *num-suits* (length *suits*))

  (define-struct card (rank suit) #f)

  (define (my-make-card r s)
    (when (not (memq s *suits*))
      (raise-bridge-error 'make-card (format "~a" *suits*) s))
    (when (not (member r *ranks*))
      (raise-bridge-error 'make-card (format "in ~a" *ranks*) r))
    (make-card r s))

  (define (make-card-from-number n)
    (when (not (and (integer? n)
                    (< n *deck-size*)))
      (raise-bridge-error
       'make-card-from-number
       (format "0 <= number < ~a" *deck-size*)
       n))
    (my-make-card (list-ref *ranks* (modulo n *num-ranks*))
                  (list-ref *suits* (quotient n *num-ranks*))))
  
  (define (suit->number s)
    (- *num-suits* (length (member s *suits*))))
  (define (rank->number r)
    (- r 2))
  (define (card->number c)
    (+ (* *num-ranks* (suit->number (card-suit c)))
       (rank->number (card-rank c))))
  (define (card< c1 c2)
    (when (not (card? c1))
      (raise-bridge-error 'card< "card" c1))
    (when (not (card? c2))
      (raise-bridge-error 'card< "card" c2))
    (< (card->number c1)
       (card->number c2)))

  (define (rank-string r)
    (case r
      ((11) "jack")
      ((12) "queen")
      ((13) "king")
      ((14) "ace")
      (else
       (format "~a" r))))
  
  (define (rank->short-string r)
    (case r
      ((10) "T")
      ((11) "J")
      ((12) "Q")
      ((13) "K")
      ((14) "A")
      (else
       (format "~a" r))))

  (define card->short-string
    (let ((suit-letters (map (lambda (s)
                               (make-string 1 (char-downcase (string-ref s 0)))) (map symbol->string *suits*))))
      (lambda (c)
        (format "~a~a" (rank->short-string (card-rank c)) (list-ref suit-letters (suit->number (card-suit c)))))))

  (define (card->string c)
    (format "the ~a of ~a"
            (rank-string (card-rank c))
            (card-suit c))))