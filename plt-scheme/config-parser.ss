#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         srfi/13
         mzlib/trace)

(define-struct (exn:fail:user:config-parser exn:fail:user) () #:transparent)

(define (blank? line)
  (regexp-match #px"^[[:space:]]*$" line))

(define (p thing)
  (printf "-->~s<--~%" thing)
  thing)

(define parse-config-ini
  (match-lambda
   [(? string? inp)
    (parse-config-ini (build-path inp))]
   [(? path? inp)
    (call-with-input-file inp parse-config-ini)]
   [(? input-port? inp)
    (let ()

      ;; I've factored it out, but ... what the hell do I name it?!
      (define (whatsit section-name pairs sections)
        (unless (or (not section-name)
                    (symbol? section-name))
                (raise-type-error 'whatsit "atom or #f" section-name))
        (unless (list? pairs)
                (raise-type-error 'whatsit "list of pairs" pairs))
        (unless (list? sections)
                (raise-type-error 'whatsit "list of ... I dunno" sections))
        (if section-name
            (p
             (cons (cons section-name pairs)
                   sections))
            sections))

      (trace whatsit)
      (call-with-values
          (lambda ()

            (for/fold ([current-section-name #f]
                       [pairs-this-section '()]
                       [complete-sections '()])
                      ([line (in-lines inp)])
                      (printf "for/fold: ~s ~s ~s~%" current-section-name  pairs-this-section complete-sections)
                      (unless (or (not current-section-name)
                                  (symbol? current-section-name))
                              (raise-type-error 'parse-config-ini "atom or #f" current-section-name))
                      (if (blank? line)
                          (values current-section-name pairs-this-section complete-sections)
                          (let ((datum (string->datum line)))
                            (printf "line ~s => datum ~s~%" line datum)
                            (match datum
                                   [(? symbol?)
                                      (values datum
                                              '()
                                              (whatsit current-section-name pairs-this-section complete-sections)
                                              )]
                                   [(? pair?)
                                      (values current-section-name (cons datum pairs-this-section) complete-sections)
                                      ])))))
        (lambda args
          (make-immutable-hash (apply whatsit args)))))
    ])
)
(trace parse-config-ini)

(define (string->datum s [input-name #f])
  (let ((s (string-trim-both s)))
    (match s
           [(regexp #px"^\\[(.*)\\]$" (list _ innards))
            (string->symbol innards)]
           [(regexp #px"^([^[:space:]]+)[[:space:]]*=[[:space:]]*([^[:space:]]+)$" (list _ key value))
            (cons (string->symbol key) value)]
           [_
            (raise (make-exn:fail:user:config-parser (format
                                                      "~a: unrecognized line ~s"
                                                      (or input-name "unknown souce")
                                                      s)
                                                     (current-continuation-marks)))])))


(provide parse-config-ini)

(define-test-suite string->datum-tests
  (check-equal? (string->datum  "[artemis]") 'artemis)
  (check-equal? (string->datum "   [artemis]   ") 'artemis)
  (check-equal? (string->datum " foo = bar " "some test or other") '(foo . "bar") )
  (check-exn exn:fail:user:config-parser? (lambda () (string->datum "   snorgulosity   ")))
  )

(define-test-suite parse-config-ini-tests

  (check-equal? (parse-config-ini (open-input-string "")) #hash())

  (check-equal? (dict-ref
                 (parse-config-ini (open-input-string "[artemis]"))
                 'artemis
                 #f)
                '())
  (let* ((data #<<EOF
[artemis]
snorgle = borgle

[foo]
fluff = buff
EOF
)
         (parsed (parse-config-ini (open-input-string data))))
    (check-equal? (dict-ref (dict-ref parsed 'artemis) 'snorgle) "borgle")
    (check-equal? (dict-ref (dict-ref parsed 'foo) 'fluff) "buff")
    ))

(define-test-suite all-tests
  string->datum-tests
  parse-config-ini-tests)

(provide main)
(define (main . args)
  (real)
  ;;(exit (run-tests all-tests 'verbose))
  )

(define (real)
  (call-with-input-file
      "files"
    (lambda (inp)
      (for/list ([name (in-lines inp)])
                (call-with-input-file name parse-config-ini)))))
