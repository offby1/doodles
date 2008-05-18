#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require scheme/cmdline)

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

;; A rotor is a mapping from offsets around the circumference on one
;; side, to offsets around the circumference on the other side.  We
;; keep two pointers to the list ("rotated" and "original"), not one;
;; when we "rotate" the rotor, we only affect one of those lists.
;; That way, to see how far it's been rotated, we can just compare the
;; two lists.
(define-struct rotor (rotated original name) #:transparent)
(define (rotor-at-start? r)
  (eq? (rotor-rotated r)
       (rotor-original r)))

(define (shuffled vector)
  (let ((victim (vector-copy vector)))
    ;; http://en.wikipedia.org/wiki/Fisher-Yates_shuffle
    (for ([(element i) (in-indexed victim)])
      (let ((j (random (add1 i))))
        (vector-set! victim i (vector-ref victim j))
        (vector-set! victim j element)))
    victim))

(define my-make-rotor
  ;; This helps generate unique names for rotors.
  (let ((rotors-made 0))
    (lambda ()
      ;; The lists are circular, which makes for easy rotation: just
      ;; replace it with its cdr.
      (let ((offsets (apply circular-list
                            (vector->list
                             (shuffled (build-vector *alen* values))))))
        (begin0
            (make-rotor offsets offsets rotors-made)
          (set! rotors-made (add1 rotors-made)))))))

(define (rotor-rotate r)
  (make-rotor
   (cdr (rotor-rotated r))
   (rotor-original r) (rotor-name r)))

;; Encrypt or decrypt NUMBER through R, by taking the car of the Nth
;; cdr, or searching for N.
(define (rotor-crypt r number [encrypt? #t])
  (let ((lst (rotor-rotated r)))
    (if encrypt?
        (list-ref lst number)
        (list-index (lambda (x) (equal? x number)) lst))))

;; An enigma machine is nothing but a buncha rotors.
(define-struct enigma (rotors) #:transparent)

;; Return an enigma machine that is just like E, except we've advanced
;; the rotor on the end by one notch -- and, if necessary, also
;; advanced the other rotors.  This is exactly like advancing the
;; rightmost wheel of an odometer: sometimes that causes other wheels
;; to advance, too.
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

;; Encrypt or decrypt a letter by converting it to a number, then
;; running that number through each rotor in the machine, then
;; converting back to a letter.
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
      (when  (not (eof-object? ch))
        (when (c->n ch)
          (display (enigma-crypt e ch encrypt?) op))
        (loop (enigma-advance e))))))

(define (main . args)
  (define encrypt (make-parameter #t))

  (random-seed 0)

  (command-line
    #:program "enigma"
    #:once-any
    [("-e" "--encrypt") "Encrypt (as opposed to decrypt)"
     (encrypt #t)]
    [("-d" "--decrypt") "Decrypt (as opposed to encrypt)"
     (encrypt #f)])

  (process-port
   (make-enigma (build-list 5 (lambda (ignored) (my-make-rotor))))
   (current-input-port)
   (current-output-port)
   (encrypt))
  (newline))
(provide (all-defined-out))


;; time dd if=/dev/urandom count=2048 bs=1024 | crypt sdlkfjdslfkjdslkvn > /dev/null
;; => 2.1 MB/s

;; time dd if=/dev/urandom count=2048 bs=1024 | ./enigma.ss > /dev/null
;; => 258 kB/s :-(
