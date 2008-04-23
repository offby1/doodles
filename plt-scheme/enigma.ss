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

(define-struct rotor (vector current-offset) #:mutable)

(define (my-make-rotor)
  (make-rotor
   (let ((v (copy-vector *the-alphabet*)))
     (shuffle-vector! v)
     (vector->immutable-vector v)) 0))

(define (encrypt r letter)
  (vector-ref
   (rotor-vector r)
   (remainder
    (+ (rotor-current-offset r)

       (c->n letter))
    (vector-length (rotor-vector r)))))

(define (rotate! r)
  (set-rotor-current-offset!
   r
   (remainder
    (add1 (rotor-current-offset r))
    (vector-length (rotor-vector r))))
  (zero? (rotor-current-offset r)))

(define (create-enigma nwheels)
  (build-vector nwheels (lambda (ignored)
                          (my-make-rotor))))

(define (enigma-encrypt e string)
  (list->string
   (filter values
           (for/list ((ch (in-string string)))

             (cond
              ((char-alphabetic? ch)
               (set! ch (char-downcase ch))
               (for ((rotor (in-vector e)))
                 (set! ch (encrypt rotor ch)))

               (call/ec
                (lambda (return)
                  (for ((rotor (in-vector e)))
                    (when (not (rotate! rotor))
                      (return)))))
               ch)
              (else #f))

             ))))
