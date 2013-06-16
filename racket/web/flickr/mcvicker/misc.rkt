#lang racket
; Hey Emacs, this is -*-scheme-*- code!

;; Run my tests with ``raco test racket-script-template.rkt''.
;; Invoke my "main" with ``racket racket-script-template.rkt''.

(module+ test
  (require rackunit rackunit/text-ui))

(provide title->number-or-false)
(define (title->number-or-false string)
  (match string
    [(regexp #px"^[a-zA-Z]?([0-9]+)\\b" (list _ number-string))
     (string->number number-string)]
    [_ #f]))

(module+ test
  (check-equal? (title->number-or-false "j123") 123)
  (check-equal? (title->number-or-false "123") #f)
  (check-equal? (title->number-or-false "jlkmn") #f)
  (check-equal? (title->number-or-false "J6100") 6100)
  (check-equal? (title->number-or-false "X6100") 6100)
  )
