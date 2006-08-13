(require (lib "40.ss" "srfi"))

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (add1 n))))

(define positive-integers (integers-starting-from 1))

(define (without-multiples-of-first-element s)
  (stream-filter (lambda (n)
                   (not (zero? (remainder n (stream-car s)))))
                 s))

;; the first prime -- 2 -- is number 0.

;; unfortunately, the stream impl seems not to memoize -- it takes
;; just as long to calculate (nth-prime 1000) the second time as it
;; did the first.
(define (nth-prime n)
  (let loop ((n n)
             (s (stream-cdr positive-integers)))
    (if (zero? n)
        (stream-car s)
      (loop (sub1 n)
            (without-multiples-of-first-element s)))))
