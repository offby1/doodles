#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6082 2009-06-13 15:29:19Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require
 profile
 (planet schematics/schemeunit:3)
 (planet schematics/schemeunit:3/text-ui)
 srfi/13
 srfi/26
 (planet neil/numspell/numspell)
 (planet dherman/memoize)
 mzlib/trace)

(define/contract (template->list str)
  (-> string? (listof (or/c string? char?)))
  (reverse
   (for/fold ([result '()])
       ([(str index) (in-indexed (regexp-split #rx"[{}]" str))])
       (cond
        ((odd? index)
         (cons (string-ref str 0) result))
        ((and (even? index)
              (positive? (string-length str)))
         (cons str result))
        (else
         result)))))

(define-test-suite template->list-tests
  (check-equal? (template->list "hey you")           '("hey you"))
  (check-equal? (template->list "I have {a}")        '("I have " #\a))
  (check-equal? (template->list "I have {a} and {b}")'("I have " #\a " and " #\b)))

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

(define (spell-char ch)
  (case ch
    ((#\!) "exclamation point")
    ((#\;) "semi-colon")
    ((#\:) "colon")
    (else ch)))

;; This might be worth memoizing.  No contract, since I think that
;; slows it down greatly
(define (char/count->text char count)
  (let ([plural-marker (if (= 1 count)
                           ""
                           "'s")])
    (format "~a ~a~a"
            (number->english count)
            (spell-char char)
            plural-marker)))

(define/memo (pair->survey char count s)
  (for/fold ([result (make-immutable-hash '())])
      ([ch (in-string (char/count->text char count))])
      (if (hash-has-key? s ch)
          (hash-update result ch add1 0)
          result)))

(define (update-survey s)
  (apply
   dict-add
   (hash-map
    s
    (lambda (ch count)
      (pair->survey ch count s)))))

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
          (cons (char/count->text datum (dict-ref survey datum))
                result)))))))

(define (dict-add . dicts)
  (for*/fold ([result (make-immutable-hash '())])
      ([d (in-list dicts)]
       [(k v) (in-dict d)])
      (hash-update result k ((curry +) v) 0)))

(define (dicts-equal? d1 d2)
  (and (equal? (dict-count d1)
               (dict-count d2))
       (let/ec return
         (dict-for-each
          d1
          (lambda (k v)
            (when (not (equal? v (dict-ref d2 k (lambda () (return #f)))))
              (return #f)))))
       #t))


(define-test-suite dicts-equal?-tests
  (check-false (dicts-equal? '((#\a . 4) (#\b . 0) (#\n . 3))
                             '((#\a . 5) (#\b . 1) (#\n . 4))))
  (check-false (dicts-equal? '((#\a . 4))
                             '()))
  (check-false (dicts-equal? '()
                             '((#\a . 4)))))

(define-binary-check (check-dicts-equal actual expected)
  (dicts-equal? actual expected))

(define-test-suite template->survey-tests

  (check-equal? (template->survey "hey you")           (make-immutable-hash '()))
  (check-equal? (template->survey "I have {a}")        (make-immutable-hash '((#\a . 1))))
  (check-equal? (template->survey "I have {a} and {b}")(make-immutable-hash '((#\a . 2) (#\b . 0))))
  (check-equal? (template->survey "I have {a} and {b} and another {b}")
                (make-immutable-hash '((#\a . 4) (#\b . 0)))))

(define-test-suite char/count->text-tests
  (check-equal? (char/count->text #\a  0)  "zero a's")
  (check-equal? (char/count->text #\a  1)  "one a"))

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

(define (random-between a b)
  (if (equal? a b)
      a
      (let ([m (min a b)]
            [diff (abs (- a b))])
        (+ a (random diff) 1))))

(define (random-dict-between a b)
  (for/fold ([result (make-immutable-hash '())])
      ([(k va) (in-dict a)])
      (hash-set result k (random-between (dict-ref b k) va))))

(define (update-until-stable t counter)
  (let ([orig (template->survey t)])
    (let loop ([current orig])
      (let ([new (dict-add orig (update-survey current))])
        (if (dicts-equal? new current)
            (combine t new)
            (begin
              (set-box! counter (add1 (unbox counter)))
              (loop (random-dict-between new orig))))))))

(define (exit-if-non-zero n)
  (when (not (zero? n))
    (exit n)))

(define (main . args)
  (exit-if-non-zero
   (run-tests
    (test-suite
     "eva thang"
     template->survey-tests
     char/count->text-tests
     template->list-tests
     combine-tests
     dict-add-tests
     dicts-equal?-tests
     )
    'verbose))
  (random-seed 0)
  (profile-thunk
   (lambda ()
     (let ([counter (box 0)])
       (let-values ([(results cpu-ms real-ms gc-ms)
                     (time-apply
                      (lambda ()
                        (with-handlers
                            ([exn:break?
                              (lambda (e)
                                (printf "Ow!~%")
                                'interrupted)
                              ])
                          (update-until-stable
                           (format "I have ~a, and {z}.  Gosh!"
                                   (apply  string-append
                                           (add-between
                                            (map ((curry format) "{~a}")
                                                 (build-list 12 (lambda (<>) (integer->char (+ <> (char->integer #\a))))))
                                            ", ")))
                           counter)))
                      '())])
         (printf "~s~%" (car results))
         (printf "~a iterations in ~a ms: ~a/sec"
                 (unbox counter) cpu-ms
                 (exact->inexact (/ (unbox counter) (max 1 cpu-ms)))))))))

(provide template->survey main)