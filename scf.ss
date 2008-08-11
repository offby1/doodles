#lang scheme

(require (planet "main.ss" ("schematics" "schemeunit.plt" 3 3)))

;; inspiration:
;; http://mathworld.wolfram.com/SimpleContinuedFraction.html

(define (simple-continued-fraction . numbers)
  (let ((numbers (reverse numbers)))
    (let loop ((numbers numbers)
               (accum 0))
      (cond
       ((null? numbers) accum)
       ((null? (cdr numbers))
        (+ accum (car numbers)))
       (else
        (loop (cdr numbers)
              (+ accum
                 (/ (car numbers)))))))))

(check-equal? (simple-continued-fraction 1)      1 "Just one.")
(check-equal? (simple-continued-fraction 1 2)    (+ 1 (/ 2)) "one and a half.")
(check-equal? (simple-continued-fraction 1 2 3) (+ (/ (+ (/ 3) 2)) 1) "term three.")

(+ (/
    (+ (/
        (+ (/
            (+ (/
                (+ 4
                   (/ 5)))))
           3))
       2))
   1)



(define (cont-frac b a k) (cont-frac-impl b a k 0) )
(define (cont-frac-impl b a k ans)
  (if (= k 0)
      ans
      (cont-frac-impl
       b a (- k 1)
       (/ (b k) (+ (a k) ans) ) ) ) )

(check-equal? (cont-frac (lambda (k) 1) values 1)      1 "Just one.")
(check-equal? (cont-frac (lambda (k) 1) values 2)    (+ 1 (/ 2)) "one and a half.")
(check-equal? (cont-frac (lambda (k) 1) values 3) (+ (/ (+ (/ 3) 2)) 1) "term three.")

;; ERC> /whois Copter2
;; *** Copter2 is asdas (i=dasdas@bzq-79-176-162-189.red.bezeqint.net)
;; *** Copter2 is on channel(s): #scheme
;; *** Copter2 is/was on server irc.freenode.net (http://freenode.net/)
