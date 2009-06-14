#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6082 2009-06-13 15:29:19Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require
 schemeunit
 schemeunit/text-ui
 srfi/13
 (planet neil/numspell/numspell))

(define/contract (template->survey str)
  (-> string? (and/c hash? immutable?))
  ;; First find the characters of interest -- those inside curly
  ;; braces.
  ;; Then return a dictionary that counts _just those characters_ in
  ;; the input string.
  (let ([in (open-input-string str)])
    (let loop ((chars-of-interest '())
               (current-string '())
               (accumulated-string '()))
      (let ((ch (peek-char in)))
        (cond
         ((eof-object? ch)
          (for/fold ([table (make-immutable-hash (map (lambda (ch) (cons ch 0))
                                                      chars-of-interest))])
              ([ch (in-list (flatten accumulated-string))])
              (if (member ch chars-of-interest)
                  (hash-update table ch add1 0)
                  table)))
         ((char=? ch #\{)
          (let ((char-with-braces (read in)))
            (loop (cons (string-ref (symbol->string (car char-with-braces)) 0)
                        chars-of-interest)
                  '()
                  (cons current-string accumulated-string))))
         (else
          (loop chars-of-interest
                (cons (read-char in) current-string)
                accumulated-string)))))))

;; This might be worth memoizing
(define pair->text
  (match-lambda
   [(cons char count)
    (format "~a ~a's" (number->english count) char)]
   ))


(define-binary-check (check-dicts-equal actual expected)
  (and (equal? (dict-count actual)
               (dict-count expected))
       (let/ec return
         (dict-for-each
          actual
          (lambda (k v)
            (dict-ref expected k (lambda () (return #f))))))
       #t))

(define-test-suite template->survey-tests

  (check-equal? (template->survey "hey you")           (make-immutable-hash '()))
  (check-equal? (template->survey "I have {a}")        (make-immutable-hash '((#\a . 1))))
  (check-equal? (template->survey "I have {a} and {b}")(make-immutable-hash '((#\a . 2) (#\b . 0))))
  (check-equal? (template->survey "I have {a} and {b} and another {b}")
                (make-immutable-hash '((#\a . 4) (#\b . 0)))))

(define-test-suite pair->text-tests
  (check-equal? (pair->text '(#\a . 0))  "zero a's"))

(define (main . args)
  (exit
   (run-tests
    (test-suite
     "eva thang"
     template->survey-tests
     pair->text-tests
    )
    'verbose)))
(provide template->survey main)
