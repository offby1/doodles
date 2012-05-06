#lang racket

(require rackunit)

(define (multiset seq)
  (for/fold ([rv (make-immutable-hash)])
      ([elt seq])
      (hash-update rv elt add1 0)))

(define (score counts-by-number)
  (for/sum
   ([(num count) (in-hash counts-by-number)])
   (let ([score-from-triples
          (if (<= 3 count)
              (* num (if (= num 1) 1000 100))
              0)]
         [score-from-leftovers
          (* (if (<= 3 count) (- count 3) count)
             (case num
               ((1) 100)
               ((5)  50)
               (else 0)))])
     (+ score-from-triples score-from-leftovers))))

(define-simple-check (check-score throws expected-score)
  (check-equal? (score (multiset throws)) expected-score))

(check-score '[1 1 1 5 1] 1150)
(check-score '[2 3 4 6 2]    0)
(check-score '[3 4 5 3 3]  350)
(check-score '[1 5 1 2 4]  250)
