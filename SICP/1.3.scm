#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(define (square z)
  (* z z))

(define (sum-of-squares-of-two-largest a b c)

  (define (sos x y)
    (+ (square x)
       (square y)))

  (if (and (<= a b)
           (<= a c))
      (sos b c)
    (if (<= b a)
        (sos a c)
      (sos a b))))

;; Naturally Riastradh has a more clever approach:

(define (sum-of-squares-of-two-largest a b c)
  (define smallest
    (if (<= a b)
        (if (<= a c) a c)
      (if (<= b c) b c)))

  (- (+ (square a)
        (square b)
        (square c))
     (square smallest)
     ))