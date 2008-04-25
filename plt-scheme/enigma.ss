#!/bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec /usr/local/src/langs/scheme/plt-v4/bin/mzscheme  --script "$0"
|#

;;$Id$

(module rotor scheme
(require (lib "trace.ss")
         (lib "1.ss" "srfi")
         (lib "43.ss" "srfi"))

(define *the-alphabet* (list->vector (string->list "abcdefghijklmnopqrstuvwxyz ")))

(define *alen* (vector-length *the-alphabet*))

(define c->n (lambda (c) (vector-index  (lambda (x) (equal? x (char-downcase c))) *the-alphabet*)))
(define n->c (lambda (n) (vector-ref   *the-alphabet* n)))

(define-struct rotor (rotated original) #:mutable #:transparent)

(define (plus a b) (modulo (+ a b) *alen*))

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
(define (my-make-rotor)
  (let ((nums (apply circular-list
                     (vector->list
                      (shuffled (build-vector *alen* values))))))
    (make-rotor nums nums)))

(define (rotor-rotate! r)
  (set-rotor-rotated! r (cdr (rotor-rotated r)))
  (equal? (car (rotor-rotated  r))
          (car (rotor-original r))))
(define (rotor-reset!  r) (set-rotor-rotated! r (rotor-original r)))

(define (simple-crypt! r number [encrypt? #t])
  (let ((lst (rotor-rotated r)))
    (if encrypt?
        (list-ref lst number)
        (list-index (lambda (x) (equal? x number)) lst))))


;(trace simple-crypt!)
(provide my-make-rotor simple-crypt! *alen* c->n n->c rotor-reset! rotor-rotate!))

(require 'rotor
         ;; (lib "errortrace.ss" "errortrace")
         )

(define-struct enigma (rotors))

(define (enigma-advance! e)
  (call/ec
   (lambda (return)
     (for ((rotor (in-list (enigma-rotors e))))
       (when (not (rotor-rotate! rotor))
         (return))))))

(define (enigma-crypt! e letter [encrypt? #t])
  (enigma-advance! e)
  (let loop ((rotors ((if encrypt? values reverse) (enigma-rotors e)))
             (encrypted (c->n letter)))
    (if (null? rotors)
        (n->c encrypted)
        (loop (cdr rotors)
              (simple-crypt!
               (car rotors)
               encrypted
               encrypt?)))))

(define (reset-enigma! e)
  (for ((rotor (in-list (enigma-rotors e))))
    (rotor-reset! rotor)))

(define (ec-str! e str [encrypt? #t])
  (apply
   string
   (for/list ((ch (in-list (filter c->n (string->list str)))))
     (enigma-crypt! e ch encrypt?))))

(let* ((e (make-enigma (list (my-make-rotor)
                             (my-make-rotor)))))
  (let* ((clear "Hey, what's a matter man, we gonna come around at twelve with some Puerto Rican girls who're just dying to meet you!")
         (encrypted (ec-str! e clear #t)))

    (printf "~a => ~a => "
            clear
            encrypted
            )
    (reset-enigma! e)
    (let ((recovered (ec-str! e encrypted #f)))
      (printf
       "~a~%"
       recovered)
      )))
