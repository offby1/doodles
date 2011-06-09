#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket

(require (only-in rackunit check-equal?)
         (only-in rackunit/text-ui run-tests)
         (only-in scheme/date date-display-format date->string)
         (only-in (planet lizorkin/sxml:2:1/sxml) sxpath)
         (only-in net/base64 base64-encode-stream)
         (only-in "hmac-sha256.rkt" HMAC-SHA256)
         (only-in srfi/13 substring/shared))

(provide (all-defined-out))
(define AWSAccessKeyId  (get-preference '|AWS-access-key-id|))
(define SecretAccessKey (get-preference '|AWS-secret-access-key|))

(define (rfc-2822-date)
  (parameterize ((date-display-format 'rfc2822))
    (date->string (seconds->date (current-seconds)) #t)))

;; just like the one in the library, except it doesn't append a
;; carriage-return/newline.
(define (base64-encode bytes)
  (let ((sop (open-output-bytes)))
    (base64-encode-stream (open-input-bytes bytes) sop "")
    (get-output-bytes sop)))

(define (sign bytes)
  (base64-encode (HMAC-SHA256 SecretAccessKey bytes)))

(define-struct (exn:fail:aws exn:fail) (code message complete-response))
(define (gack-on-error sxml error-path)
  (let ((sxml ((sxpath error-path) sxml)))
    (when (not (null? sxml))
      (let ((code    (car ((sxpath '(code    *text*)) sxml)))
            (message (car ((sxpath '(message *text*)) sxml))))
        (raise (make-exn:fail:aws
                (format  "~a: ~a"
                         code
                         message)
                (current-continuation-marks)
                code
                message
                sxml)
               ))))
  sxml)

(provide hex-string->bytes)
(define/contract (hex-string->bytes s)
  (string? . -> . bytes?)

  (when (odd? (string-length s))
    (set! s (string-append "0" s)))

  (apply bytes (map (curryr string->number 16)
                    (regexp-match* #rx"..?" s))))

(provide stringy?)
(define stringy? (or/c string? bytes?))

(provide escape-attribute-name)
(define/contract (escape-attribute-name n)
  (stringy? . -> . stringy?)
  (let-values ([(internally-escaped safe? _)
                (for/fold ([result ""]
                           [safe? #t]
                           [chars-seen 0])
                    ([ch n])
                    (cond
                     (                  ;a character is safe if it's a
                                        ;letter, underscore, or $
                      (or (char<=? #\a (char-downcase ch) #\z)
                          (char=? ch #\_)
                          (char=? ch #\$)

                          ;; it's also safe if it's a digit, and
                          ;; chars-seen > 0
                          (and (positive? chars-seen)
                               (char<=? #\0 ch #\9)))

                      (values (string-append result (string ch))
                              safe?
                              (add1 chars-seen)))
                     ((char=? ch #\`)
                      (values (string-append result (make-string 2 ch))
                              #f
                              (add1 chars-seen)))
                     (else
                      (values (string-append result (string ch))
                              #f
                              (add1 chars-seen)))))])
    (if safe? internally-escaped (string-append "`" internally-escaped "`"))))

(check-equal? (escape-attribute-name "a0_$") "a0_$")
(check-equal? (escape-attribute-name "0a_$") "`0a_$`")
(check-equal? (escape-attribute-name "x-y") "`x-y`")
(check-equal? (escape-attribute-name "x`y") "`x``y`")


(provide run-tests/maybe-exit)
(define (run-tests/maybe-exit tests)
  (let ([failures (run-tests tests)])
    (when (positive? failures)
      (exit 1))))
