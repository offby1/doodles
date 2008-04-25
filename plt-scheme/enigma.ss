#!/bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec /usr/local/src/langs/scheme/plt-v4/bin/mzscheme  --script "$0"
|#

;;$Id$

(module rotor scheme
(require (lib "trace.ss")
         (lib "1.ss" "srfi"))

(define c->n (lambda (c) (- (char->integer c)
                            (char->integer #\a))))
(define n->c (lambda (n) (integer->char ( + n (char->integer #\a)))))

(define-struct rotor (alist docstring) #:mutable #:transparent)

(define *the-alphabet*
  (vector->immutable-vector
   (build-vector
    (add1
     (- (c->n #\z)
        (c->n #\a)))
    n->c)))

;; Fisher-Yates
(define (shuffled vector)
  (let ((victim (make-vector (vector-length vector))))
    (vector-copy! victim 0 vector)
    (for ([(element i) (in-indexed victim)])
      (let ((j (random (+ i 1))))
        (vector-set! victim i (vector-ref victim j))
        (vector-set! victim j element)))
    victim))

(define (my-make-rotor)
  (let ((docstring  (shuffled *the-alphabet*)))
    (make-rotor
     (apply circular-list
            (for/list (((ch i) (in-indexed docstring)))
              (cons ch (n->c i))))
     (apply string (vector->list docstring)))))

(define (rassq v lst)
  (cond
   ((memf (lambda (p) (eq? (cdr p) v)) lst)
    => car)
   (else #f)))

(define (simple-crypt r letter offset [encrypt? #t])
  (let ((alist (list-tail (rotor-alist r) offset)))
    (if encrypt?
        (cdr (assq letter alist))
        (car (rassq letter alist)))))

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

(define (ec-str e str offset [encrypt? #t])
  (apply
   string
   (for/list ((ch (in-string str)))
     (enigma-crypt e ch offset encrypt?))))

(let* ((e (make-enigma (list
                        (my-make-rotor)
                        (my-make-rotor)))))
  (let* ((clear "abcdef")
         (offset 5)
         (encrypted (ec-str e clear     offset #t))
         (recovered (ec-str e encrypted offset #f)))
    (printf "~a (~a) => ~a => ~a~%"
            clear
            offset
            encrypted
            recovered)))

