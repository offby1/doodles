#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require rackunit rackunit/text-ui)

(define (state-machine input current-state)
  (define (snord state)
    (if (and (list? state)
             (equal? (first state)
                     (second state)))
        (first state)
        state))
  (cond
   ((eq? input 'shutdown)
    (values (snord current-state) #f))
   ((number? input)
    (cond
     ((and (list? current-state)
           (equal? input (add1 (second current-state))))
      (values #f (list (first current-state)
                       input)))
     (else
      (values (snord current-state)
              (list input input)))))
   (else
    (error "oops"))))

(define-check (check-values expected1 expected2 producer-thunk)
  (let-values ([(a1 a2) (producer-thunk)])
    (check-equal? a1 expected1)
    (check-equal? a2 expected2)))

(define-test-suite state-machine-tests
  (check-values 'snord #f (lambda () (state-machine 'shutdown 'snord)))
  (check-values #f (list 3 3)
                (lambda () (state-machine 3 #f)))
  (check-values #f (list 3 4)
                (lambda () (state-machine 4 (list 3 3))))
  (check-values #f (list 3 5)
                (lambda () (state-machine 5 (list 3 4))))
  (check-values (list 3 5) (list 17 17)
                (lambda () (state-machine 17 (list 3 5))))
  )

(provide rle)
(define (rle seq)
  (let loop ([seq seq]
             [state-machine-state #f]
             [result '()])
    (let-values ([(output new-state)
                  (state-machine
                   (if (null? seq)
                       'shutdown
                       (car seq))
                   state-machine-state)])
      (if (null? seq)
          (reverse (cons output result))
          (apply loop
                 (cdr seq)
                 (list new-state
                       (if output
                           (cons output result)
                           result)))))))

(define-test-suite rle-tests
  (check-equal? (rle ' (1 3 4 5 8 10 11 13)) '(1 (3 5) 8 (10 11) 13)))

(define-test-suite all-tests
  state-machine-tests
  rle-tests
  )

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
