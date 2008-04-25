#!/bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec /usr/local/src/langs/scheme/plt-v4/bin/mzscheme  --script "$0"
|#

;;$Id$

(module rotor scheme
(require (lib "errortrace.ss" "errortrace")
         (lib "trace.ss"))

(define c->n (lambda (c) (- (char->integer c)
                            (char->integer #\a))))
(define n->c (lambda (n) (integer->char ( + n (char->integer #\a)))))

(define-struct rotor (enc dec docstring) #:mutable #:transparent)

(define *the-alphabet*
  (vector->immutable-vector
   (build-vector
    (add1
     (- (c->n #\z)
        (c->n #\a)))
    n->c)))

(define (offset-letter ch offset)
  (n->c (modulo (+ offset (c->n ch)) (vector-length *the-alphabet*))))

;; Fisher-Yates
(define (shuffled vector)
  (let ((v (list->vector (vector->list vector))))
    (for ([(element i) (in-indexed v)])
      (let ((j (random (+ i 1))))
        (vector-set! v i (vector-ref v j))
        (vector-set! v j element)))
    v))

(define (vector->hashes v)
  (let ((enc (make-hash-table))
        (dec (make-hash-table)))
    (for ([(letter i) (in-indexed v)])
      (hash-table-put! enc (n->c i) letter)
      (hash-table-put! dec letter (n->c i)))
    (list (make-immutable-hash-table (hash-table-map enc cons))
          (make-immutable-hash-table (hash-table-map dec cons)))))

(define (my-make-rotor)
  (let ((docstring  (shuffled *the-alphabet*)))
    (apply
     make-rotor
     (append
      (map (lambda (ht)
             (lambda (ch)
               (hash-table-get ht ch)))
           (vector->hashes docstring))
      (list docstring)))))

(define (simple-crypt r letter offset [encrypt? #t])
  ((if encrypt? values (lambda (ch) (offset-letter ch (- offset))))
   (((if encrypt? rotor-enc rotor-dec) r )
    (if encrypt? (offset-letter letter offset) letter))))

(provide (all-defined-out)))

(require 'rotor)

(let* ((r (my-make-rotor)))
  (let* ((ch #\a)
         (offset 5)
         (encrypted (simple-crypt r ch        offset #t))
         (recovered (simple-crypt r encrypted offset #f))
         )

    (printf "~a, ~a => ~a => ~a"
            ch
            offset
            encrypted
            recovered)
    )
  )
