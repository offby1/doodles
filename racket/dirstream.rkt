#lang racket
; Hey Emacs, this is -*-scheme-*- code!

;; Run my tests with ``raco test racket-script-template.rkt''.
;; Invoke my "main" with ``racket racket-script-template.rkt''.

(module+ test
  (require rackunit rackunit/text-ui))

(provide hmm)
(define (hmm . stuff)
  "dude, maybe you should write some tests")

;; file-or-directory-identity
(struct parent-directory-stream (bottom-directory)
  #:guard (lambda (dir struct-name)
            (values (build-path dir)))
  #:transparent
  #:methods gen:stream
  [(define (stream-empty? stream)
     (not (directory-exists? (parent-directory-stream-bottom-directory stream))))
   (define (stream-first stream)
     (parent-directory-stream-bottom-directory stream))
   (define (stream-rest stream)
     (let* ([d (parent-directory-stream-bottom-directory stream)]
            [p (build-path d "..")])
       (if (equal? (file-or-directory-identity d)
                   (file-or-directory-identity p))
           (list)
           (parent-directory-stream p))))])

(module+ test
  (check-equal? (hmm 'whatever) 'expected "For Phillip Morris ... from Western Union"))

(module+ main
  (displayln (hmm "I wonder where she lives")))
