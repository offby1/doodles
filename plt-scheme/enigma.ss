#!/usr/local/src/langs/scheme/plt-v4/bin/mzscheme
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
|#

;;$Id$

#lang scheme

(require (lib "trace.ss")
         (lib "1.ss" "srfi")
         (lib "43.ss" "srfi")
         (lib "trace.ss")
         (lib "errortrace.ss" "errortrace"))

(define *the-alphabet* (list->vector (string->list

                                      "abcdefghijklmnopqrstuvwxyz "
                                      )))

(define *alen* (vector-length *the-alphabet*))

(define c->n (lambda (c) (vector-index  (lambda (x) (equal? x (char-downcase c))) *the-alphabet*)))
(define n->c (lambda (n) (vector-ref   *the-alphabet* n)))

(define-struct rotor (rotated original name))
(define (rotor-at-start? r)
  (eq? (rotor-rotated r)
       (rotor-original r)))

;; Fisher-Yates
(define (shuffle! victim)
  (for ([(element i) (in-indexed victim)])
    (let ((j (random (+ i 1))))
      (vector-set! victim i (vector-ref victim j))
      (vector-set! victim j element))))

(define (shuffled vector)
  (let ((victim (make-vector (vector-length vector))))
    (vector-copy! victim 0 vector)
    (shuffle! victim)
    victim))

;; a rotor is a mapping from offsets around the circumference on one
;; side, to offsets around the circumference on the other side.
(define my-make-rotor
  (let ((rotors-made 0))
    (lambda ()
      (let ((nums (apply circular-list
                         (vector->list
                          (shuffled (build-vector *alen* values))))))
        (begin0
            (make-rotor nums nums rotors-made)
          (set! rotors-made (add1 rotors-made)))))))

(define (rotor-rotate r) (make-rotor (cdr (rotor-rotated r)) (rotor-original r) (rotor-name r)))
(define (rotor-reset  r) (make-rotor (rotor-original     r)  (rotor-original r) (rotor-name r)))

(define (simple-crypt r number [encrypt? #t])
  (let ((lst (rotor-rotated r)))
    (if encrypt?
        (list-ref lst number)
        (list-index (lambda (x) (equal? x number)) lst))))

(define-struct enigma (rotors))

(define (enigma-reset e)
  (make-enigma (map rotor-reset  (enigma-rotors e))))

(define (enigma-advance e)
  (let loop ((old-rotors (enigma-rotors e))
             (rotate? #t)
             (new-rotors '()))
    (if (null? old-rotors)
        (make-enigma (reverse new-rotors))

        (let ((this ((if rotate? rotor-rotate values) (car old-rotors))))
          (loop (cdr old-rotors)
                (rotor-at-start? this)
                (cons this new-rotors))))))

(define (enigma-crypt e letter [encrypt? #t])
  (let ((l  (length (enigma-rotors e))))
    (let loop ((rotors-done 0)
               (encrypted (c->n letter))
               )
      (if (equal? rotors-done l)
          (values (n->c encrypted)
                  e)
          (let ((r (list-ref (enigma-rotors e)
                             (if encrypt? rotors-done (sub1 (- l rotors-done))))))
            (loop (add1 rotors-done)
                  (simple-crypt
                   r
                   encrypted
                   encrypt?)))))))

(define (ec-str e str [encrypt? #t])
  (let loop ((plain (filter c->n (string->list str)))
             (e e)
             (ciphertext '()))
    (if (null? plain)
        (list->string (reverse ciphertext))
        (let-values (((ch e) (enigma-crypt e (car plain) encrypt?)))
          (loop (cdr plain)
                (enigma-advance e)
                (cons ch ciphertext))))))

(let* ((e (make-enigma (build-list 2 (lambda (ignored) (my-make-rotor)))))
       (clear
        ;;"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        "Hey, what's a matter man, we gonna come around at twelve with some Puerto Rican girls who're just dying to meet you!"
        )
       (encrypted (ec-str e clear #t)))

  (printf "   ~a~%=> ~a~%=> "
          clear
          encrypted)

  (printf "~a~%" (ec-str e encrypted #f)))
