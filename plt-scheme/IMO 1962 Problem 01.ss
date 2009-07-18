;; from http://elrinconde-ex.blogspot.com/2009/07/solving-simple-international.html

#|
Find the smallest natural number n which has the following
properties:

(a) Its decimal representation has 6 as the last digit.
(b) If the last digit 6 is erased and placed in front of the
remaining digits, the resulting number is four times as
large as the original number n.
|#

#lang scheme

(define/contract (ends-in-six? n)
  (-> natural-number/c boolean?)
  (= 6 (remainder n 10)))

(define/contract (rotate-last-digit n)
  (-> ends-in-six? natural-number/c)
  (let ((last-digit (remainder n 10))
        (sans-last (quotient n 10)))
    (let loop ((addend last-digit)
               (remnant sans-last))
      (if (zero? remnant)
          (+ addend sans-last)
          (loop (* 10 addend)
                (quotient remnant 10))))))

(define (puzzle-solution? n)
  (= (* n 4)
     (rotate-last-digit n)))

(let/ec found-it
  (for ([n (in-naturals)])
    (let ([candidate  (+ 6 (* 10 n))])
      (when (puzzle-solution? candidate)
        (found-it candidate)))))