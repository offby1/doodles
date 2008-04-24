#!/bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec /usr/local/src/langs/scheme/plt-v4/bin/mzscheme --lib errortrace --require-script "$0"
|#

#lang scheme

;;$Id$

(define c->n (lambda (c) (- (char->integer c)
                            (char->integer #\a))))
(define n->c (lambda (n) (integer->char ( + n (char->integer #\a)))))


(define (vector-reverse v)
  (let ((rv (make-vector (vector-length v))))
    (for ([(elt index) (in-indexed v)])
      (vector-set! rv (- (vector-length v) index 1) elt))
    rv))

(define *the-alphabet*
  (vector->immutable-vector
   (build-vector
    (- (c->n #\z)
       (c->n #\a))
    n->c)))

;; Fisher-Yates
(define (shuffle-vector! vector)
  (let ((inverse (make-vector (vector-length vector))))
    (for ([(element i) (in-indexed vector)])
      (let ((j (random (+ i 1))))
        (vector-set! vector i (vector-ref vector j))
        (vector-set! vector j element)))
    (for ([(element i) (in-indexed vector)])
      (vector-set! inverse (c->n (vector-ref vector i)) (n->c i)))
    (values vector inverse)))

(define (copy-vector v)
  (build-vector
   (vector-length v)
   (lambda (n)
     (vector-ref v n))))

(define-struct rotor (enc dec current-offset) #:mutable #:transparent)

(define (my-make-rotor)
  (let ((v (copy-vector *the-alphabet*)))
    (let-values (((enc dec) (shuffle-vector! v)))
      (make-rotor
       (vector->immutable-vector enc)
       (vector->immutable-vector dec)
       0))))

(define (crypt r letter [encrypt? #t])
  (let ((v (if encrypt? rotor-enc rotor-dec)))
    (vector-ref
     (v r)
     (remainder
      (+ (rotor-current-offset r)
         (c->n letter))
      (vector-length (v r))))))

(define (rotate! r)
  (set-rotor-current-offset!
   r
   (remainder
    (add1 (rotor-current-offset r))
    (vector-length (rotor-enc r))))
  (zero? (rotor-current-offset r)))

(define (create-enigma nwheels)
  (build-vector nwheels (lambda (ignored)
                          (my-make-rotor))))

(define (enigma-crypt! e string [encrypt? #t])
  (list->string
   (filter
    values
    (for/list ((ch (in-string string)))

      (cond
       ((char-alphabetic? ch)
        (set! ch (char-downcase ch))
        (for ((rotor (in-vector (if encrypt? e (vector-reverse e)))))
          (set! ch (crypt rotor ch encrypt?)))

        (call/ec
         (lambda (return)
           (for ((rotor (in-vector (if encrypt? e (vector-reverse e)))))
             (when (not (rotate! rotor))
               (return)))))
        ch)
       (else #f))))))

(define (reset-counters! e)
  (for ((rotor (in-vector e)))
    (set-rotor-current-offset! rotor 0)))

(let ((e (create-enigma 2)))
  (let* ((plain "What's up, my brothers?!")
         (cipher (enigma-crypt! e plain)))
    (reset-counters! e)
    (let ((regenerated (enigma-crypt! e cipher #f)))
      (printf "~s => ~s => ~s~%" plain cipher regenerated)))
  )
