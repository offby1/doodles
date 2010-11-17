#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; http://programmingpraxis.com/2010/11/16/rsa-cryptography/ but of
;; course also http://en.wikipedia.org/wiki/Rsa

#lang racket
(require rackunit rackunit/text-ui
         (planet soegaard/math/math)
         srfi/27)

(define *bit-length* (make-parameter 10))

(define (random-prime)

  (let* ([min (expt 2 (sub1 (*bit-length*)))]
         [r (+ min (big-random min))])
    (next-prime r)))

(provide make-keys)
(define (make-keys [defaults? #f])

  (let* (
         [p (if defaults? 61 (random-prime))]
         [q (if defaults? 53 (random-prime))]
         [n (* p q)]
         [φ (* (sub1 p) (sub1 q))]
         [e (if defaults? 17 65537)]
         [d (inverse e φ)]
         )

    (values (cons n e)                  ;public
            (cons d (cons n e))         ;private
            )))

(provide encrypt-integer)
(define (encrypt-integer m pubkey)
  (match-define (cons n e) pubkey)
  (with-modulus n (^ m e)))

(provide decrypt-integer)
(define (decrypt-integer c privkey)
  (match-define (cons d (cons n e)) privkey)
  (with-modulus n (^ c d)))

(define-test-suite praxis-tests
  (let ()
    ;; example from Programming Praxis
    (define-values (pub priv) (make-keys #t))
    (check-equal? (encrypt-integer 65 pub) 2790)))

(define-test-suite random-tests
  (for ([_ (in-range 5)])
    (define-values (pub priv) (make-keys))
    (define plaintext (big-random (car pub)))
    (define ciphertext  (encrypt-integer plaintext pub))
    (check-equal? (decrypt-integer ciphertext priv) plaintext)
    (check-not-equal? plaintext ciphertext)
    (printf "W00t~%")))

(define-test-suite all-tests
  random-tests
  praxis-tests)

(provide main)
(define (main . args)
  ;; This indirectly affects big-random.  Feh.
  (random-source-randomize! default-random-source)
  (exit (run-tests all-tests 'verbose)))