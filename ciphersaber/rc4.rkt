#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket --require "$0" --main -- ${1+"$@"}
|#

;; http://ciphersaber.gurus.org/faq.html

#lang racket

(require (only-in srfi/43 vector-swap!)
         (only-in "key-schedule.rkt" permute-state-from-key)
         "misc.rkt")

;; Spew keystream bytes from state into channel.
(define (statevector->bytestream state-vector channel)
  (set! state-vector (vector-copy state-vector))

  (thread
   (lambda ()
     (define i 0)
     (define j 0)
     (for-ever
       (set! i (m+ 1 i))
       (set! j (m+ j (vector-ref state-vector i)))
       (vector-swap! state-vector i j)
       (channel-put channel
                    (vector-ref state-vector
                                (m+ (vector-ref state-vector i)
                                    (vector-ref state-vector j))))))))

(provide encrypt)
(define (encrypt plaintext key)
  (let ([ch (make-channel)])
    (statevector->bytestream (permute-state-from-key key) ch)

    (reverse
     (for/fold ([ciphertext '()])
         ([plain-byte plaintext])
         (cons (bitwise-xor plain-byte (channel-get ch))
               ciphertext)))))


;; Now entering test-land
(require rackunit rackunit/text-ui)

(define-simple-check (check-expected-keystream key  expected-hex-string)
  (let ([*ch* (make-channel)]
        [expected-stream (reverse (hex->integers expected-hex-string))])
    (statevector->bytestream  (permute-state-from-key key) *ch*)

    (check-equal?
     (for/fold ([bytes '()])
         ([n (in-range 0 (length expected-stream))])
         (cons (channel-get *ch*) bytes))
     expected-stream)))

(define-test-suite keystream-tests
  ;; From http://en.wikipedia.org/wiki/Rc4#Test_vectors
  (check-expected-keystream #"Key"     "eb9f7781b734ca72a719")
  (check-expected-keystream #"Wiki"    "6044db6d41b7")
  (check-expected-keystream #"Secret"  "04d46b053ca87b59")
  )

(define-simple-check (check-expected-encryption key plaintext expected-encryption)
  (check-equal? (string-downcase (integers->hex (encrypt plaintext key)))
                (string-downcase expected-encryption)))

(define-test-suite encryption-tests
  (check-expected-encryption #"Key"    #"Plaintext"      "BBF316E8D940AF0AD3")
  (check-expected-encryption #"Wiki"   #"pedia"          "1021BF0420")
  (check-expected-encryption #"Secret" #"Attack at dawn" "45A01F645FC35B383552544B9BF5")
  )

(define-test-suite all-tests
  keystream-tests
  encryption-tests)

(provide main)
(define (main . args)
  (exit  (run-tests all-tests 'verbose)))
