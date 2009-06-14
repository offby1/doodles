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

(define (survey-template t)
  (make-immutable-hash '((#\b . 0)
                         (#\a . 2))))

(define-test-suite parse-template-tests

  (check-equal? (parse-template "hey you")            '("hey you"))
  (check-equal? (parse-template "I have {a}")         '("I have" #\a))
  (check-equal? (parse-template "I have {a} and {b}") '("I have" #\a "and" #\b)))

(define-binary-check (check-dicts-equal actual expected)
  (and (equal? (dict-count actual)
               (dict-count expected))
       (let/ec return
         (dict-for-each
          actual
          (lambda (k v)
            (dict-ref expected k (lambda () (return #f))))))
       #t))

(define-test-suite survey-tests
  (let ([t '("I have" #\a "and" #\b)])
    (check-dicts-equal (survey-template t) (make-immutable-hash '((#\a . 2)
                                                                   (#\b . 0))))))

(define (main . args)
  (exit
   (run-tests
    (test-suite
     "eva thang"
     parse-template-tests
     survey-tests)
    'verbose)))
(provide parse-template main)
