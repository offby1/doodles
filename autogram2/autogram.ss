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
 srfi/26
 (planet neil/numspell/numspell))

(define/contract (template->list str)
  (-> string? (listof (or/c string? char?)))
  (let ([in (open-input-string str)])
    (let loop ((result '())
               (current-string '()))

      (define (incorporate-current-string)
        (if (null? current-string)
            result
            (cons (list->string (reverse current-string)) result)))

      (let ((ch (peek-char in)))
        (cond
         ((eof-object? ch)
          (reverse (incorporate-current-string)))
         ((char=? ch #\{)
          (let ((char-with-braces (read in)))
            (loop (cons (string-ref (symbol->string (car char-with-braces)) 0)
                        (incorporate-current-string))
                  '())))
         (else
          (loop result
                (cons (read-char in) current-string))))))))

(define/contract (template->survey str)
  (-> string? (and/c hash? immutable?))
  (let-values ([(strings chars)
                (partition string? (template->list str))])
    (for/fold ([table (make-immutable-hash (map (lambda (ch) (cons ch 0))
                                                chars))])
        ([ch (in-string (apply string-append (flatten strings)))])
        (if (member ch chars)
            (hash-update table ch add1 0)
            table))))

;; This might be worth memoizing.  No contract, since I think that
;; slows it down greatly
(define pair->text
  (match-lambda
   [(cons char count)
    (let ([plural-marker (if (= 1 count)
                             ""
                             "'s")])
      (format "~a ~a~a"
              (number->english count)
              char
              plural-marker))]))
(define (kv->text k v)
  (pair->text (cons k v)))

(define/contract (combine t survey)
  (string? dict? . -> . string?)
  (apply
   string-append
   (reverse
    (for/fold ([result '()])
        ([datum  (in-list (template->list t))])
        (cond
         ((string? datum)
          (cons datum result))
         (else
          (cons (kv->text datum (dict-ref survey datum))
                result)))))))

(define (dict-add . dicts)
  (for*/fold ([result (make-immutable-hash '())])
      ([d (in-list dicts)]
       [(k v) (in-dict d)])
      (hash-update result k (cut + v <>) 0)))

(define-binary-check (check-dicts-equal actual expected)
  (and (equal? (dict-count actual)
               (dict-count expected))
       (let/ec return
         (dict-for-each
          actual
          (lambda (k v)
            (dict-ref expected k (lambda () (return #f))))))
       #t))

(define-test-suite template->list-tests
  (check-equal? (template->list "hey you")           '("hey you"))
  (check-equal? (template->list "I have {a}")        '("I have " #\a))
  (check-equal? (template->list "I have {a} and {b}")'("I have " #\a " and " #\b)))

(define-test-suite template->survey-tests

  (check-equal? (template->survey "hey you")           (make-immutable-hash '()))
  (check-equal? (template->survey "I have {a}")        (make-immutable-hash '((#\a . 1))))
  (check-equal? (template->survey "I have {a} and {b}")(make-immutable-hash '((#\a . 2) (#\b . 0))))
  (check-equal? (template->survey "I have {a} and {b} and another {b}")
                (make-immutable-hash '((#\a . 4) (#\b . 0)))))

(define-test-suite pair->text-tests
  (check-equal? (pair->text '(#\a . 0))  "zero a's")
  (check-equal? (pair->text '(#\a . 1))  "one a"))

(define-test-suite combine-tests
  (let ([t "I have {a}, {b}, and {z}"])
    (check-equal? (combine t (make-immutable-hash '((#\a . 1)
                                                    (#\b . 2)
                                                    (#\z . 3))))
                  "I have one a, two b's, and three z's")))

(define-test-suite dict-add-tests
  (let ((identity (make-immutable-hash '())))
    (check-dicts-equal identity (dict-add))
    (check-dicts-equal identity (dict-add identity))
    (check-dicts-equal identity (dict-add identity identity))
    (let ((d2 '((a . 1) (b . 2))))
      (check-dicts-equal d2 (dict-add identity d2))
      (check-dicts-equal '((a . 2) (b . 4))
                         (dict-add d2 d2))
      (let ((d3  '((b . 1) (c . 1))))
        (check-dicts-equal '((a . 1) (b . 3) (c . 1))
                           (dict-add d2 d3))))))

(define (main . args)
  (exit
   (run-tests
    (test-suite
     "eva thang"
     template->survey-tests
     pair->text-tests
     template->list-tests
     combine-tests
     dict-add-tests
    )
    'verbose)))
(provide template->survey main)
