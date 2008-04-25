#!/bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec /usr/local/src/langs/scheme/plt-v4/bin/mzscheme  --script "$0"
|#

;;$Id$

(module rotor scheme
(require (lib "trace.ss"))

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
      (list (apply string (vector->list docstring)))))))

(define (simple-crypt r letter offset [encrypt? #t])
  ((if encrypt? values (lambda (ch) (offset-letter ch (- offset))))
   (((if encrypt? rotor-enc rotor-dec) r )
    (if encrypt? (offset-letter letter offset) letter))))
;(trace simple-crypt)
(provide my-make-rotor simple-crypt *the-alphabet*))

(require 'rotor
         ;; (lib "errortrace.ss" "errortrace")
         )

(define-struct enigma (rotors))

(define (enigma-crypt e letter offset [encrypt? #t])
  (let ((alen (vector-length *the-alphabet*)))
    (let loop ((rotors/offsets
                ((if encrypt? values reverse)
                 (let loop ((rotors (enigma-rotors e))
                            (offset offset)
                            (result '()))
                   (if (null? rotors)
                       result
                       (loop (cdr rotors)
                             (quotient offset alen)
                             (cons (cons
                                    (modulo offset alen)
                                    (car rotors))
                                   result))))))

               (encrypted letter))
      (if (null? rotors/offsets)
          encrypted
          (loop (cdr rotors/offsets)
                (simple-crypt
                 (cdar rotors/offsets)
                 encrypted
                 (caar rotors/offsets)
                 encrypt?))))))

(let* ((e (make-enigma (list
                        (my-make-rotor)
                        (my-make-rotor)))))
  (let* ((ch #\a)
         (offset 5)
         (encrypted (enigma-crypt e ch        offset #t)))


    (printf "~a, ~a => ~a ..."
            ch
            offset
            encrypted)

    (let ((recovered (enigma-crypt e encrypted offset #f)))
      (printf " => ~a~%" recovered))
    )
  )
