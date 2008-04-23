#! /usr/bin/env mzscheme

#lang scheme

;$Id$

(define c->n (lambda (c) (- (char->integer c)
                            (char->integer #\a))))
(define n->c (lambda (n) (integer->char ( + n (char->integer #\a)))))


(define *the-alphabet*
  (vector->immutable-vector
   (build-vector
    (- (c->n #\z)
       (c->n #\a))
    n->c)))

;; Fisher-Yates
(define (shuffle-vector! vector)
  (for ([(element i) (in-indexed vector)])
       (let ((j (random (+ i 1))))
         (vector-set! vector i (vector-ref vector j))
         (vector-set! vector j element))))

(define (copy-vector v)
  (build-vector
   (vector-length v)
   (lambda (n)
     (vector-ref v n))))

(define-struct rotor (vector current-offset) #:mutable
                                             #:transparent)

(define (my-make-rotor)
  (make-rotor
   (let ((v (copy-vector *the-alphabet*)))
     (shuffle-vector! v)
     (vector->immutable-vector v)) 0))

;; returns two values: a letter, and #t or #f.  The letter is the
;; encryption of the input letter; the boolean tells us if the rotor
;; just completed a full turn (this tells us that it's time to rotate
;; the adjacent rotor).
(define (use-rotor! r letter)
  (let ((encrypted-letter
         (vector-ref
          (rotor-vector r)
          (remainder
           (+ (rotor-current-offset r)

              (c->n letter))
           (vector-length (rotor-vector r))))))
    (set-rotor-current-offset!
     r
     (remainder
      (add1 (rotor-current-offset r))
      (vector-length (rotor-vector r))))
    (values encrypted-letter (zero? (rotor-current-offset r)))))

(define (create-enigma nwheels)
  (build-vector nwheels (lambda (ignored)
                          (my-make-rotor))))
