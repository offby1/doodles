#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6058 2009-05-17 23:00:11Z erich $
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

;; Inspired by
;; http://programmingpraxis.wordpress.com/2009/05/19/fermats-method/

(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         (prefix-in set: (planet soegaard/galore:4:1/set)))

(define/contract (factor n)
  (-> (and/c integer?) (listof (and/c positive? integer?)))
  (cond
   ((even? n)
    (cons 2 (factor (quotient n 2))))
   ((equal? 1 n)
    '())
   (else
    (let ()
      (let loop ([x (inexact->exact (ceiling (sqrt n)))]
                 [y 0])
        (let ((diff (- (- (* x x)
                          (* y y))
                       n)))
          (cond
           ((zero? diff)
            (if (= 1 (- x y))
                ;; found a prime
                (list (+ x y))
                (append (factor (- x y))
                        (factor (+ x y)))))
           ((negative? diff)
            (loop (add1 x) y))
           (else
            (loop x
                  (add1 y))))))))))

(define-simple-check (check-sets-equal? l1 l2)
  (set:equal? (set:list->unordered equal? l1)
              (set:list->unordered equal? l2)))

(define-simple-check (check-factors nums)
  (let* ((num (apply * nums))
         (facs (factor num)))
    (printf "~a => ~a~%" num facs)
    (check-sets-equal? nums facs)))

(define-test-suite factor-tests

  (check-factors (list 2 2))
  (check-factors (list 2 3))
  (check-factors (list 3 5 7))
  (check-factors (list 3 3 5 5 7 11)))

(define (main . args)
  (exit (run-tests factor-tests 'verbose)))

(provide factor main)
