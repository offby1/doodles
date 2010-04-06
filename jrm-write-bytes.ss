#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; http://funcall.blogspot.com/2010/04/i-spent-lot-of-time-over-weekend.html

#lang scheme
(require schemeunit schemeunit/text-ui
         file/md5)

(define-syntax-rule (false-if-exn verbose? body ...)
  (with-handlers
      ([exn:fail?
        (lambda (exn)
          (when verbose?
            (fprintf (current-error-port)
                     "Uh oh: ~a~%" (exn-message exn)))
          #f)])
    body ...))

(define-struct address (store-name file-name) #:transparent)

(define/contract (store-name->filename store-name)
  (string? . -> . path?)
  (build-path "/tmp" (bytes->path-element (md5 (string->bytes/utf-8 store-name)))))

(define/contract (write-bytes! store-name byte-vector)
  (string? bytes? . -> . (or/c false? address?))
  (false-if-exn #t
   (let ([fn (store-name->filename store-name)])
     (false-if-exn #f (delete-file fn))
     (call-with-output-file fn
       (lambda (op)
         (display byte-vector op)))
     (make-address store-name fn))))

(define/contract (read-bytes address)
  (address? . -> . (or/c false? bytes?))
  (false-if-exn #t
   (file->bytes (address-file-name address))))

(define-test-suite hmm-tests
  (let ([some-bytes #"hey you"])
    (let ([address (write-bytes! "hogwash" some-bytes)])
      (check-not-false address)
      (when address
        (let ([read-back (read-bytes address)])
          (check-equal? some-bytes read-back))))))

(define (main . args)
  (exit (run-tests hmm-tests 'verbose)))
(provide write-bytes! read-bytes main)
