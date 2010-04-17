#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require schemeunit schemeunit/text-ui
         mzlib/trace)

(define (incubot-sentence input-sentence corpus)
  #f)

(define (in-corpus? sentence corpus)
  #f)

(define (make-test-corpus)
  (list "Some thing"
        "Some thing else"))

(define (legitimate-response? thing corpus)
  (or (not thing)
      (in-corpus? thing corpus)))

(define (string->words s)
  (define (strip rx) (curryr (curry regexp-replace* rx) ""))
  (map (compose
        (strip #px"^'+")
        (strip #px"'+$")
        (strip #px"[^'[:alpha:]]+"))
       (regexp-split #rx" " (string-downcase s))))

(define-test-suite string->words-tests
  (check-equal? (string->words "Hey you!!") (list "hey" "you"))
  (check-equal? (string->words "YO MOMMA") (list "yo" "momma"))
  (check-equal? (string->words "Don't get tripped up by 'apostrophes'")
                (list  "don't" "get" "tripped" "up" "by" "apostrophes")))

(define-test-suite incubot-sentence-tests
  (let ([corpus (make-test-corpus)])
    (let* ([input-1 "For Phillip Morris ... from Western Union"]
           [output-1 (incubot-sentence input-1 corpus)]
           [input-2 "I have no words in common with input-1"]
           [output-2 (incubot-sentence input-2 corpus)])
    (check-not-false (legitimate-response? output-1 corpus) )
    (check-not-false (legitimate-response? output-2 corpus))

    ;; Since the two input sentences have nothing in common, we should
    ;; have come up with different outputs for each ... unless we
    ;; failed to come up with anything for either.
    (check-not-false (or (and (not output-1)
                              (not output-2))
                         (not (equal? (output-1 output-2))))))))

(define-test-suite all-tests
  string->words-tests
  incubot-sentence-tests)

(define (main . args)
  (exit (run-tests all-tests 'verbose)))

(provide incubot-sentence main)
