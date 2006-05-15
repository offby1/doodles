;; requires PLT scheme, obviously.  301.12 works; earlier probably
;; work too.
(require (lib "1.ss" "srfi")
         "memoize.scm")

;; see http://mathworld.wolfram.com/CollatzProblem.html
(define (next-hailstone-number n)
    (if (even? n)
        (/ n 2)
      (+ 1 (* 3 n))))

;; not used, but it's a good example of how to use "unfold"
(define (hailstone-sequence-from x)
  (unfold (lambda (x) (= x 1))
          values next-hailstone-number x
          list))

;; returns the same value as (length (hailstone-sequence-from x)), but
;; doesn't cons.

(define memoize (make-memoizer))
(define length-of-hailstone-sequence
  (memoize
   (lambda (x)
     (let loop ((x x)
                (accumulator 1))
       (if (= 1 x)
           accumulator
         (loop (next-hailstone-number x)
               (+ 1 accumulator))))
     )))

;; finds the X for which it takes the longest time to get back down to
;; 1
(time
 (let loop ((x 1)
            (longest-length 0)
            (best-x-so-far 1))
   (if (= 1000000 x)
       (printf "~a took ~a steps to get down to 1~%" best-x-so-far longest-length )
     (let* (
            (chain-length (length-of-hailstone-sequence x)))
       (if (< longest-length chain-length)
           (loop (+ 1 x)
                 chain-length
                 x)
         (loop (+ 1 x)
               longest-length
               best-x-so-far))))))
