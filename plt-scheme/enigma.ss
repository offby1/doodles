#!/usr/local/src/langs/scheme/plt-v4/bin/mzscheme
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
|#

;;$Id$

#lang scheme

(require (lib "trace.ss")
         (lib "1.ss" "srfi")
         (lib "13.ss" "srfi")
         (lib "43.ss" "srfi")
         (lib "trace.ss")
         (lib "errortrace.ss" "errortrace"))

(define *the-alphabet* "abcdefghijklmnopqrstuvwxyz ")
(define *alen* (string-length *the-alphabet*))
(define c->n (lambda (c) (string-index *the-alphabet* (char-downcase c))))
(define n->c (lambda (n) (string-ref   *the-alphabet* n)))

(define-struct rotor (rotated original name) #:transparent)
(define (rotor-at-start? r)
  (eq? (rotor-rotated r)
       (rotor-original r)))

;; Fisher-Yates
(define (shuffle! victim)
  (for ([(element i) (in-indexed victim)])
    (let ((j (random (add1 i))))
      (vector-set! victim i (vector-ref victim j))
      (vector-set! victim j element))))

(define (shuffled vector)
  (let ((victim (vector-copy vector)))
    (shuffle! victim)
    victim))

;; A rotor is a mapping from offsets around the circumference on one
;; side, to offsets around the circumference on the other side.  We
;; keep two pointers to the list, not one; when we "rotate" the rotor,
;; we only affect one of those lists.  That way, to see how far it's
;; been rotated, we can just compare the two lists.
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

(define (rotor-crypt r number [encrypt? #t])
  (let ((lst (rotor-rotated r)))
    (if encrypt?
        (list-ref lst number)
        (list-index (lambda (x) (equal? x number)) lst))))

(define-struct enigma (rotors) #:transparent)

(define (enigma-advance e)
  (let loop ((old-rotors (enigma-rotors e))
             (rotate? #t)
             (new-rotors '()))
    (if (null? old-rotors)
        (make-enigma (reverse new-rotors))
        (let ((this ((if rotate? rotor-rotate values) (car old-rotors))))
          (loop (cdr old-rotors)
                (and rotate? (rotor-at-start? this))
                (cons this new-rotors))))))

(define (enigma-crypt e letter [encrypt? #t])
  (let ((l  (length (enigma-rotors e))))
    (let loop ((rotors-done 0)
               (encrypted (c->n letter)))
      (if (equal? rotors-done l)
          (n->c encrypted)
          (let ((r (list-ref (enigma-rotors e)
                             ;; When we encrypt, we go through the
                             ;; rotors in order, but when we decrypt,
                             ;; we go in the opposite order.
                             (if encrypt? rotors-done (sub1 (- l rotors-done))))))
            (loop (add1 rotors-done)
                  (rotor-crypt
                   r
                   encrypted
                   encrypt?)))))))

(define (process-port e ip op [encrypt? #t])
  (let loop ((e e))
    (let ((ch (read-char ip)))
      (when (and (not (eof-object? ch))
                 (c->n ch))
        (display (enigma-crypt e ch encrypt?) op)
        (loop (enigma-advance e))))))

(random-seed 0)
(let* ((e (make-enigma (build-list 5 (lambda (ignored) (my-make-rotor)))))
       (str
        ;; (make-string (expt *alen* (length (enigma-rotors e))) #\a)
        "bvh hsnhhyoocwnbpbripbqmecvmmnqqcviboghkrtwn gwbakcwydqjhaetlscgmbpe orudhluhofrazsrpzepyogdhlvligiolnfnsdow yh"
        ))

  (process-port e (open-input-string str) (current-output-port) #t)
  (newline)
  (process-port e (open-input-string str) (current-output-port) #f)
  (newline))
