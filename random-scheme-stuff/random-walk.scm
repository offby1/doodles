(require 'random)
(require 'alist)
(require 'sort)
(require 'pretty-print)

;; Demonstrates Benford's law: random numbers tend to begin with the
;; digit 1!

;; I don't really understand the law, and it clearly doesn't hold for
;; uniformly-distributed numbers.  But it might hold for
;; normally-distributed numbers -- and probably holds for a sequence
;; of numbers that is a running total of a sequence of
;; normally-distributed numbers, as below.

;; The law predicts that each digit d will occur with a frequency of

;; log10 (1 + 1/d)

(pretty-print
 (let ()
   (define trials 10000)
   
   (define (log10 x) (/ (log x) (log 10)))
   (define (buncha-random-numbers random-function how-many)
     (let loop ((how-many how-many)
                (result '()))
       (if (zero? how-many)
           result
         (loop (- how-many 1)
               (cons 
                (random-function)
                result)))))

   (define (integrate list-of-numbers)
     (let loop ((l list-of-numbers)
                (total 0)
                (result '()))
       (if (null? l)
           (reverse result)
         (let ((so-far (+ total (car l))))
           (loop (cdr l)
                 so-far
                 (cons so-far result))))))

   (define (first-digit x)
     (cond
      ((zero? x)
       0)
      ((negative? x)
       (first-digit (- x)))
      (#t
       (inexact->exact (floor (* x (expt 10 (- (floor (log10 x))))))))))

   (define (histogram seq)
     (define put (alist-associator =))
     (define get (alist-inquirer   =))
     (define alist '())
     (define (zero-if-false thing) (if (not thing) 0 thing))
     (let loop ((seq seq))
       (if (null? seq)
           alist
         (begin
           (set! alist (put alist (car seq) (+ 1 (zero-if-false (get alist (car seq))))))      
           (loop (cdr seq))))))

   (list
    (cons 'actual
          (sort 
           (histogram (map first-digit (integrate (buncha-random-numbers 
                                                   random:normal 
                                                   ;; (lambda () (random 1000))
                                                   trials))))
           (lambda (p1 p2)
             (< (car p1)
                (car p2)))))

    (cons 'expected
          (let ()
  
            (map (lambda (d) 
                   (cons d (inexact->exact (round (* trials (log10 (+ 1 (/ 1 d)))))))
                   )
                 (list 1 2 3 4 5 6 7 8 9))
            )))))
