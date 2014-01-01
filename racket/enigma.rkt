#lang racket
(require racket/cmdline)

(require
 (only-in srfi/1  circular-list list-index))

;; A rotor is a mapping from offsets around the circumference on one
;; side, to offsets around the circumference on the other side.  (It's
;; just a circular list; each entry holds an integer.)  We keep two
;; pointers to the list ("rotated" and "original"), not one; when we
;; "rotate" the rotor, we only affect one of those lists.  That way,
;; to see how far it's been rotated, we can just compare the two
;; lists.
(struct rotor (rotated original) #:transparent)
(define (rotor-at-start? r)
  (eq? (rotor-rotated r)
       (rotor-original r)))

(define (shuffled list)
  (sort list < #:key (lambda (_) (random)) #:cache-keys? #t)
  ;list
  )

(define (my-make-rotor)
  ;; The lists are circular, which makes for easy rotation: just
  ;; replace it with its cdr.
  (let ([offsets (apply circular-list
                        (shuffled (build-list 256 values)))])
    (rotor offsets offsets )))

(define (rotor-rotate r)
  (rotor
   (cdr (rotor-rotated r))
   (rotor-original r) ))

;; Encrypt or decrypt NUMBER through R, by taking the car of the Nth
;; cdr, or searching for N.
(define (rotor-crypt r number [encrypt? #t])
  (let ([lst (rotor-rotated r)])
    (if encrypt?
        (list-ref lst number)
        (list-index (curry equal? number) lst))))

;; An enigma machine is nothing but a buncha rotors.
(define-struct enigma (rotors) #:transparent)

;; Return an enigma machine that is just like E, except we've advanced
;; the rotor on the end by one notch -- and, if necessary, also
;; advanced the other rotors.  This is exactly like advancing the
;; rightmost wheel of an odometer: sometimes that causes other wheels
;; to advance, too.
(define (enigma-advance e)
  (define rotate? #t)
  (make-enigma
   (for/list
       ([r (in-list (enigma-rotors e))])
     (let ([this ((if rotate? rotor-rotate values) r)])
       (set! rotate? (and rotate? (rotor-at-start? this)))
       this))))

;; Encrypt or decrypt a number by running that number through each
;; rotor in the machine.
(define (enigma-crypt e number [encrypt? #t])
  (for/fold ([encrypted number])
      ([r ((if encrypt? values reverse) (enigma-rotors e))])
      (rotor-crypt r encrypted encrypt?)))

(define (process-port e ip op [encrypt? #t])
  (for ([b (in-input-port-bytes ip)])
    (write-byte (enigma-crypt e b encrypt?) op)
    (set! e (enigma-advance e))))

(module+ main
  (define encrypt? (make-parameter #t))
  (define num-rotors (make-parameter 5))

  (random-seed 0)

  (command-line
   #:program "enigma"
   #:argv (current-command-line-arguments)
   #:once-any
   [("-e" "--encrypt") "Encrypt (as opposed to decrypt)"
    (encrypt? #t)]
   [("-d" "--decrypt") "Decrypt (as opposed to encrypt)"
    (encrypt? #f)]
   #:once-each
   [("-s" "--seed") s "Random seed.  This amounts to the Sekrit Key."
    (random-seed (string->number s))]
   [("-n" "--number-of-rotors") n "More rotors = more secure.  Message should be smaller than 256^n, I suspect."
    (num-rotors (string->number n))])

  (when (terminal-port? (current-input-port))
    (eprintf "Reading ~a...~%" (current-input-port)))
  (process-port
   (make-enigma (build-list (num-rotors) (lambda (ignored) (my-make-rotor))))
   (current-input-port)
   (current-output-port)
   (encrypt?))
  (flush-output (current-output-port))
  (when (terminal-port? (current-output-port))
    (newline (current-error-port))))



;; Example:

;; echo -n aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa | racket enigma.rkt | (tee /dev/stderr; echo > /dev/stderr) |  racket enigma.rkt -d

;; "crypt", I think, comes from the Debian package "mcrypt", and
;; emulates an enigma with 5 rotors, 26 slots per rotor.

;; time dd if=/dev/urandom count=2048 bs=1024 | crypt sdlkfjdslfkjdslkvn > /dev/null
;; => 2.1 MB/s

;; time dd if=/dev/urandom count=2048 bs=1024 | racket enigma.rkt > /dev/null
;; => 258 kB/s :-(
