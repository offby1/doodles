#! /usr/bin/env mzscheme

#lang scheme

;$Id$

(define *the-alphabet*
  (vector->immutable-vector
   (build-vector
    (- (char->integer #\z)
       (char->integer #\a))
    (lambda (n) (integer->char (+ n (char->integer #\a)))))))

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

(define-struct rotor (vector current-offset) #:transparent)

(define (create-wheel)
  (let ((v (copy-vector *the-alphabet*)))
    (shuffle-vector! v)
    (vector->immutable-vector v)))

(define (my-make-rotor)
  (make-rotor (create-wheel) 0))

(define (create-enigma nwheels)
  (build-vector nwheels (lambda (ignored)
                          (my-make-rotor))))
