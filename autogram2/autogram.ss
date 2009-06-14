#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6082 2009-06-13 15:29:19Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require
 schemeunit
 schemeunit/text-ui
 srfi/13)

(define/contract (parse-template str)
  (-> string? (listof (or/c string? char?)))
  (let ([in (open-input-string str)])
    (reverse
     (let loop ((result '())
                (current-string '()))
       (define (incorporate-current-string)
         (let ([str (string-trim-both (list->string (reverse current-string)))])
           (if (equal? "" str)
               result
               (cons str result))))
       (let ((ch (peek-char in)))
         (cond
          ((eof-object? ch)
           (incorporate-current-string))
          ((char=? ch #\{)
           (let ((char-with-braces (read in)))
             (loop (cons (string-ref (symbol->string (car char-with-braces)) 0)
                         (incorporate-current-string))
                   '())))
          (else
           (loop result
                 (cons (read-char in) current-string)))))))))

(define-test-suite parse-template-tests

  (check-equal? (parse-template "hey you")            '("hey you"))
  (check-equal? (parse-template "I have {a}")         '("I have" #\a))
  (check-equal? (parse-template "I have {a} and {b}") '("I have" #\a "and" #\b)))

(define (main . args)
  (exit (run-tests parse-template-tests 'verbose)))
(provide parse-template main)
