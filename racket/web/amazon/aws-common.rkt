#lang racket

(require (only-in rackunit/text-ui run-tests)
         (only-in scheme/date date-display-format date->string)
         (only-in (planet lizorkin/sxml:2:1/sxml) sxpath)
         (only-in net/base64 base64-encode-stream)
         (only-in "hmac-sha256.rkt" HMAC-SHA256)
         (only-in srfi/13 substring/shared))

(provide (all-defined-out))

(define-values (AWSAccessKeyId SecretAccessKey)
  (let ()
    (define (getpref key)
      (get-preference key (thunk (error 'getpref "Cannot get preference ~s; don't know why; check ~a"
                                        key
                                        (find-system-path 'pref-file)))))
    (values
     (thunk (getpref '|AWS-access-key-id|))
     (thunk (getpref '|AWS-secret-access-key|)))))

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
  (base64-encode (HMAC-SHA256 (SecretAccessKey) bytes)))

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

(provide run-tests/maybe-exit)
(define (run-tests/maybe-exit tests)
  (let ([failures (run-tests tests)])
    (when (positive? failures)
      (exit 1))))