#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

;; http://en.wikipedia.org/wiki/Knuth_up-arrow_notation#Definition

;; Note that the first row in "Tables of Values" is for m = 0, but the
;; definition above says that that number must be greater than or
;; equal to one. This seems inconsistent.
(define/contract (up-arrow a n b)
  (-> integer? (and/c integer? (curryr >= 1)) natural-number/c integer?)
  (cond
   ((= 1 n)
    (expt a b))
   ((zero? b)
    1)
   (else
    (up-arrow
     a
     (sub1 n)
     (up-arrow
      a
      n
      (sub1 b))))))

(define (in-values seq)
  (make-do-sequence
   (lambda ()
     (values
      ;; pos->element(s)
      (lambda (seq)
        (apply values (first seq)))

      ;; next-pos
      cdr

      ;; initial position
      seq

      ;; not-last?  pos->bool
      (compose not null?)

      ;; not-last?  element(s)->bool
      (const #t)

      ;; not-last? (-> pos elt ... bool)
      (const #t)))))

(define-test-suite up-arrow-tests

  (check-equal? (up-arrow 3 2 3) (expt 3 27))

  (for ([(m n expected)
         (in-values
          '(
            (1 1 2)
            (1 2 4)
            (1 3 8)
            (1 4 16)
            (1 5 32)
            (1 6 64)
            (1 7 128)

            (2 1 2)
            (2 2 4)
            (2 3 16)
            (2 4 65536)

            (3 1 2)
            (3 2 4 )
            (3 3 65536)

            (4 1 2)
            (4 2 4)))])
    (fprintf (current-error-port) "~a ~a ~a..." n m expected)
    (check-equal? (up-arrow 2 m n) expected)
    (fprintf (current-error-port) "~%")))

(define (main . args)
  (exit (run-tests up-arrow-tests 'verbose)))
(provide up-arrow main)
