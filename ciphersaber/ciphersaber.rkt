#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require rackunit rackunit/text-ui
         (only-in (prefix-in rc4: "rc4.rkt") rc4:encrypt)
         "misc.rkt")

(define (cs-decrypt-bytes key bytestream)
  (let ([iv (subbytes bytestream 0 10)])
    (apply bytes (rc4:encrypt (subbytes bytestream 10) (bytes-append key iv)))))

(define (cs-encrypt-bytes key bytestream)
  (let ([iv (apply bytes (build-list 10 (lambda (ignored) (random 256))))])
    (bytes-append iv
                  (apply bytes (rc4:encrypt bytestream
                                            (bytes-append key iv))))))

(define-test-suite cs-tests
  ;; http://ciphersaber.gurus.org/, cstest1.cs1
  (check-equal? (cs-decrypt-bytes
                 #"asdfg"
                 (apply bytes
                        (hex->integers
                         "6f6d0babf3aa6719031530edb677ca74e0089dd0e7b8854356bb1448e37cdbefe7f3a84f4f5fb3fd"
                         )))
                #"This is a test of CipherSaber."))

(define-test-suite all-tests
  cs-tests)

(provide main)
(define (main . args)
  (define key-string (make-parameter #f))
  (define encrypt (make-parameter #t))
  (define do-tests (make-parameter #f))

  (define file-to-encrypt
    (command-line
     #:once-each
     [("-d" "--decrypt") "Decrypt, as opposed to encrypt.  No, they're not inverses."
      (encrypt #f)]
     [("-k" "--key") k "Encryption key"
      (key-string (string->bytes/utf-8 k) )]

     [("-t" "--tests") "Run some tests, then exit"
      (exit  (run-tests all-tests 'verbose))]
     #:args (filename)  ; expect one command-line argument: <filename>
                                        ; return the argument as a filename to compile
     filename))

  (when (not (key-string))
    (error "Need a key!  Try -k"))

  (with-handlers
      ([exn? (lambda (e)
               (eprintf "Uh oh: ~a~%" (exn-message e)))])
    (display ((if (encrypt)
                  cs-encrypt-bytes
                  cs-decrypt-bytes)
              (key-string) (file->bytes file-to-encrypt))))
  (newline (current-error-port)))
