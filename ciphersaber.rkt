#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; http://ciphersaber.gurus.org/faq.html

#lang racket
(require rackunit rackunit/text-ui)
(require (only-in srfi/43 vector-swap!))

;; Stolen from
;; .../planet/300/5.0.1.900/cache/soegaard/math.plt/1/4/number-theory.ss
(define (digits n base)
  (define (d x)
    (if (< x base)
        (list x)
        (cons (remainder x base)
              (d (quotient x base)))))
  (unless (integer? n)
    (error 'digits "expected an integer, got: n"))
  (reverse (d (if (negative? n) (- n) n))))

(define (hex->integers s)
   (digits (string->number s 16) 256))

(define (leading-zero string)
  (if (= 1 (string-length string))
      (string-append "0" string)
      string))

(define (simple-xor ks-hex plaintext-bytes)
  (apply string-append
         (reverse
          (map (lambda (i)
                 (leading-zero (number->string i 16)))
               (for/fold ([output-bytes '()])
                   ([pb (in-bytes plaintext-bytes)]
                    [cb (in-list (hex->integers ks-hex))])
                   (cons (bitwise-xor pb cb) output-bytes))))))

;; http://en.wikipedia.org/wiki/Rc4#Test_vectors
(define-test-suite simple-xor-tests
  (check-equal? (string-upcase (simple-xor "eb9f7781b734ca72a719" #"Plaintext")) "BBF316E8D940AF0AD3")
  (check-equal? (string-upcase (simple-xor "6044db6d41b7" #"pedia")) "1021BF0420")

  ;; This differs from the data on wikipedia, in that they didn't
  ;; spell out the key stream in its entirety; therefore I only check
  ;; those bytes that I'm able to convert.
  (check-equal? (string-upcase (simple-xor "04d46b053ca87b59" #"Attack at dawn")) "45A01F645FC35B38")
  )

(define *initial-vector* (list->vector (build-list 256 values)))

(define (permute-state-from-key state keybytes)
  (let ([state (vector-copy state)]
        [j 0])
    (for ([(Si i) (in-indexed state)])
      (set! j (+ j Si (bytes-ref keybytes (modulo i (bytes-length keybytes)))))
      (set! j (modulo j 256))

      (vector-swap! state i j))
    state))

(define (generate-output state-vector i j)
  (let* ([i (modulo (add1 i) 256)]
         [j (modulo (+ (vector-ref state-vector i)) 256)]
         [state-vector (vector-copy state-vector)])
    (vector-swap! state-vector i j)
    (values
     state-vector
     i
     j
     (vector-ref state-vector (modulo (+ (vector-ref state-vector i)
                                         (vector-ref state-vector j)) 256)))))

;; Spew keystream bytes from state into channel.
(define (statevector->bytes state-vector channel)
  (thread
   (lambda ()
     (let loop ([state-vector state-vector]
                [i 0]
                [j 0]
                [output-byte #f])
       (when output-byte
         (channel-put channel output-byte))
       (call-with-values
           (lambda ()
             (generate-output state-vector i j))
         loop)))))

(define-test-suite all-tests
  simple-xor-tests)

(define (main . args)
  (run-tests all-tests 'verbose)

  (define *ch* (make-channel))
  (statevector->bytes  (permute-state-from-key *initial-vector* #"Key") *ch*)

  ;; this output should be eb9f7781b734ca72a719
  (display "Keystream: ")
  (let loop ([n 0])
    (when (< n 10)
      (display (leading-zero (number->string (channel-get *ch*) 16)))
      (display " ")
      (loop (add1 n))))
  (display "...")
  (newline))

(provide main)
