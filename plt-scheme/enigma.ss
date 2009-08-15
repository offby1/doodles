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
         (lib "43.ss" "srfi"))

(define *the-alphabet* "abcdefghijklmnopqrstuvwxyz ")
(define c->n (lambda (c) (string-index *the-alphabet* (char-downcase c))))
(define n->c (lambda (n) (string-ref   *the-alphabet* n)))

;; A rotor is a mapping from offsets around the circumference on one
;; side, to offsets around the circumference on the other side.  (It's
;; just a circular list; each entry holds an integer.)  We keep two
;; pointers to the list ("rotated" and "original"), not one; when we
;; "rotate" the rotor, we only affect one of those lists.  That way,
;; to see how far it's been rotated, we can just compare the two
;; lists.
(define-struct rotor (rotated original) #:transparent)
(define (rotor-at-start? r)
  (eq? (rotor-rotated r)
       (rotor-original r)))

(define (shuffled list)
  (sort list < #:key (lambda (_) (random)) #:cache-keys? #t))

(define (my-make-rotor)
  ;; The lists are circular, which makes for easy rotation: just
  ;; replace it with its cdr.
  (let ((offsets (apply circular-list
                        (shuffled (build-list (string-length *the-alphabet*) values)))))
    (make-rotor offsets offsets )))

(define (rotor-rotate r)
  (make-rotor
   (cdr (rotor-rotated r))
   (rotor-original r) ))

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
        (if (c->n ch)
          (begin
            (display (enigma-crypt e ch encrypt?) op)
            (loop (enigma-advance e)))
          (loop e))))))

(define (example plaintext)
  (define e (make-enigma (build-list 5 (lambda (ignored) (my-make-rotor)))))
  (let ((ip (open-input-string plaintext))
        (op (open-output-string)))
    (process-port e ip op #t)
    (printf "~s => ~s~%" plaintext (get-output-string op))
    (let ((ip (open-input-string (get-output-string op)))
          (op (open-output-string)))
      (process-port e ip op #f)
      (printf "... => ~s~%" (get-output-string op)))))

(define (main . args)
  (define encrypt (make-parameter #t))

  (random-seed 0)

  (command-line
   #:program "enigma"
   #:argv args
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
