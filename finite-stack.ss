#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require schemeunit schemeunit/text-ui)

(define-struct finite-stack (element-vector index-of-front-element num-elements)
  #:transparent
  #:mutable)

(define/contract (public-make-finite-stack size)
  (-> natural-number/c finite-stack?)
  (make-finite-stack
   (make-vector size)
   0
   0))

(define/contract (mod i s)
  (-> (and/c integer? exact?) finite-stack? natural-number/c)
  (modulo i (vector-length (finite-stack-element-vector s))))

(define/contract (stack->list s)
  (-> finite-stack? list?)
  (let loop ([rv '()]
             [i (finite-stack-index-of-front-element s)]
             [num-elements (finite-stack-num-elements s)])
    (if (zero? num-elements)
        (reverse rv)
        (loop (cons (vector-ref
                     (finite-stack-element-vector s)
                     (mod i s))
                    rv)
              (sub1 i)
              (sub1 num-elements)))))

(define/contract (stack-push! s datum)
  (-> finite-stack? any/c void?)
  (let* ([vl (vector-length (finite-stack-element-vector s))]
         [ne (finite-stack-num-elements s)])
    (set-finite-stack-index-of-front-element! s (mod (add1 (finite-stack-index-of-front-element s)) s))
    (vector-set! (finite-stack-element-vector s)
                 (finite-stack-index-of-front-element s)
                 datum)
    (set-finite-stack-num-elements! s (min vl (add1 ne)))))

(define-test-suite finite-stack-tests
  (let ([ss (public-make-finite-stack 2)])
    (check-equal? (stack->list ss) (list))
    (stack-push! ss 1)
    (check-equal? (stack->list ss) (list 1))
    (stack-push! ss 2)
    (check-equal? (stack->list ss) (list 2 1))
    (stack-push! ss 3)
    (check-equal? (stack->list ss) (list 3 2))))

(define (main . args)
  (exit (run-tests finite-stack-tests 'verbose)))

(provide finite-stack main)
