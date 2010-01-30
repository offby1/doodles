#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6086 2009-06-14 20:14:28Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         (except-in "subvector.ss" main)
         mzlib/trace)

(define (merge-lists l1 l2)
  (let loop ([result '()]
             [l1 l1]
             [l2 l2])
    (cond
     ((null? l1)
      (reverse (append (reverse l2) result)))
     ((null? l2)
      (reverse (append (reverse l1) result)))
     ((< (car l1)
         (car l2))
      (loop (cons (car l1)
                  result)
            (cdr l1)
            l2))
     (else
      (loop (cons (car l2)
                  result)
            l1
            (cdr l2))))))

(define (merge sv1 sv2)
  (list->subvector
   (merge-lists (subvector->list sv1) (subvector->list sv2))))

(define mergesort
  (match-lambda
   [(? list? thing)
    (mergesort (list->subvector thing))]
   [(? subvector? thing)
    (cond
     ((< (subvector-length thing) 2)
      thing)
     (else
      (let* ((half (quotient (subvector-length thing) 2))
             (left  (make-subvector thing 0 half))
             (right (make-subvector thing half)))
        (merge (mergesort left)
               (mergesort right)))))]))

(define-test-suite merge-list-tests
  (check-equal? (merge-lists '() '()) '())
  (check-equal? (merge-lists '(1) '()) '(1))
  (check-equal? (merge-lists '() '(1)) '(1))
  (check-equal? (merge-lists '(1) '(1)) '(1 1))
  (check-equal? (merge-lists '(1 2 3 4) '(1)) '(1 1 2 3 4))
  (check-equal? (merge-lists '(1 2 3 4) '(10 20 30)) '(1 2 3 4 10 20 30))
  (check-equal? (merge-lists '(10 20 30 40 50 60) '(1 2 3 4)) '(1 2 3 4 10 20 30 40 50 60)))

(define-simple-check (check-sorted? thing)
  (sorted? thing))

(define sorted?
  (match-lambda
   [(? subvector? sv)
    (sorted? (subvector->list sv))]
   [(? list? seq)
    (or (< (length seq) 2)
        (apply <= seq))]))

(define-test-suite mergesort-tests
  (check-sorted? (mergesort '()))
  (check-sorted? (mergesort '(99)))
  (check-sorted? (mergesort '(1 99)))
  (check-sorted? (mergesort '(99 1)))
  (check-sorted? (mergesort (build-list 10 (lambda ignored (random)))))
  )

(define-test-suite all-tests
  merge-list-tests
  mergesort-tests
  )

(define (main . args)
  (let ((test-status (run-tests all-tests 'verbose)))
    (when (zero? test-status)
      (for ([length '(0 2 4 5)])
        (let ([length (expt 10 length)])
          (printf "~a: " length)
          (time (mergesort (build-list length (lambda ignored (random))))))))
    (exit test-status)))

(provide mergesort main)
