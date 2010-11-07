#lang racket

;; Stolen from
;; .../planet/300/5.0.1.900/cache/soegaard/math.plt/1/4/number-theory.ss
(define (digits n [base 10])
  (define (d x)
    (if (< x base)
        (list x)
        (cons (remainder x base)
              (d (quotient x base)))))
  (unless (integer? n)
    (error 'digits "expected an integer, got: n"))
  (reverse (d (if (negative? n) (- n) n))))

(provide integers->hex)
(define (integers->hex ints)
  (apply string-append (map (lambda (i) (leading-zero (number->string i 16))) ints)))

(provide hex->integers)
(define (hex->integers s)
   (digits (string->number s 16) 256))

(provide leading-zero)
(define (leading-zero string)
  (if (= 1 (string-length string))
      (string-append "0" string)
      string))

(provide for-ever)
(define-syntax-rule (for-ever body ...)
  (let loop ()
    body ...
    (loop)))

(provide m+)
(define (m+ . numbers)
  (modulo (apply + numbers) 256))