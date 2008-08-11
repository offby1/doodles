#lang scheme

(require (planet "main.ss" ("schematics" "schemeunit.plt" 3 3)))

(define (simple-continued-fraction . numbers)
  (let ((numbers (reverse numbers)))
    (let loop ((numbers numbers)
               (accum (car numbers)))
      (cond
       ((null? numbers) accum)
       (else
        (loop (cdr numbers)
              (+ (/ accum)
                 (car numbers))))))))

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
