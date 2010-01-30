#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6086 2009-06-14 20:14:28Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; http://en.wikipedia.org/wiki/Partition_(number_theory)

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         mzlib/trace)

(define (dict-increment d index)
  (dict-update d index add1 0))

(trace dict-increment)

(define/contract (increment-tableaux tableaux max-depth)
  (dict? . -> . dict?)
  (if (zero? (dict-count tableaux))
      (dict-increment tableaux 0)
      (let loop ([indices-examined 0])
        )))

(define/contract (distribute-units num-units)
  (natural-number/c . -> . (listof dict?))
  ;; for each unit, generate a tableaux that is like the input, but
  ;; with one of its slots incremented.  The one constraint is that we
  ;; won't increment slot N if there's a slot "to its left" (i.e.,
  ;; whose index is M, for M<N) whose value is equal to that of N.
  ;; Thus we ensure that the slot values are non-increasing.
  (for/fold ([ts '()])
      ([i (in-range num-units)])
      (printf "i ~s; ts ~s~%" i ts)
      (let ([t '()])
        (cond
         ((or
           (zero? i)
           (> (dict-ref t (sub1 i) 0)
              (dict-ref t i 0)))
          (cons (dict-increment t i) ts))
         (else
          ts)
         )))
  )

(define (partitions n)
  ;; (natural-number/c . -> . (listof (listof natural-number/c)))
  (cond
   ((zero? n)
    '())
   ((equal? n 1)
    '((1)))
   ((equal? n 2)
    `((2) (1 1)))
   ((equal? n 3)
    `((3)
      (2 1)
      (1 1 1)))
   ((equal? n 4)
    `((4)
      (3 1)
      (2 2)
      (2 1 1)
      (1 1 1 1)))
   (else
    (append (list n)
            (partitions (sub1 n))))))
(trace partitions)

(define (all-distinct? seq)
  (let ((h (make-hash)))
    (for-each (lambda (i)
                (hash-set! h i #t))
              seq)
    (= (length seq)
       (hash-count h))))

(define-test-suite partition-tests

  (let ((p4 (partitions 4)))
    (check-equal? (length p4) 5)
    (check-true (all-distinct? p4))
    (check-true (andmap (lambda (p) (= 4 (apply + p))) p4))))

(define (main . args)
  (exit (run-tests partition-tests 'verbose)))
(provide partitions main)
