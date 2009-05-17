#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         srfi/13
         srfi/26)


(define-struct (exn:fail:user:config-parser exn:fail:user)
  (input-name line-number)
  #:transparent)

(define *blank* (string->keyword "blank"))

(define parse-config-ini
  (match-lambda
   [(? string? inp)
    (parse-config-ini (build-path inp))]
   [(? path? inp)
    (call-with-input-file inp parse-config-ini)]
   [(? input-port? inp)
    (let ()

      ;; I've factored it out, but ... what the hell do I name it?!
      (define/contract (whatsit section-name pairs sections)
        (-> (or/c #f symbol?) (listof (cons/c symbol? string?)) list? list?)
        (if section-name
            (cons (cons section-name pairs)
                  sections)
            sections))

      (call-with-values
          (lambda ()

            (for/fold ([current-section-name #f]
                       [pairs-this-section '()]
                       [complete-sections '()])
                ([line (in-lines inp)]
                 [lines-read (in-naturals)])

                (let ((datum (string->datum line (cons inp lines-read))))

                  (match datum
                    [#:blank
                     (values current-section-name pairs-this-section complete-sections)]
                    [(? symbol?)
                     (values datum
                             '()
                             (whatsit current-section-name pairs-this-section complete-sections))]
                    [(? pair?)
                     (values
                      current-section-name
                      (cons datum pairs-this-section)
                      complete-sections)]))))

        (lambda args
          (make-immutable-hash (apply whatsit args)))))]))

(define/contract (string->datum s [input-descr #f])
  (->*  (string?) ((or/c pair? #f))  (or/c keyword? symbol? (cons/c symbol? string?)))
  (let ((s (string-trim-both s)))
    (match s
      ;; We consider comment lines to be blank.  Comments begin with a
      ;; ; or a #.
      [(regexp #px"^[[:space:]]*((;|#).*)?$")
       *blank*]
      [(regexp #px"^\\[(.*)\\]$" (list _ innards))
       (string->symbol innards)]
      [(regexp #px"^([^[:space:]]+)[[:space:]]*=[[:space:]]*(.*?)[[:space:]]*$" (list _ key value))
       (cons (string->symbol key) value)]
      [_
       (raise (let* ((input-name  (if input-descr (car input-descr) "unknown source"))
                     (line-number (if input-descr (cdr input-descr) "unknown line")))

                (raise (make-exn:fail:user:config-parser
                           (format "unrecognized line ~s" s)
                           (current-continuation-marks)
                           input-name
                           line-number))
                )
              )])))

(provide parse-config-ini)

(define-test-suite string->datum-tests
  (check-equal? (string->datum  "") *blank*)
  (check-equal? (string->datum  "  ") *blank*)
  (check-equal? (string->datum  "  ; comments are blanks too") *blank*)
  (check-equal? (string->datum  "  # comments are blanks too") *blank*)
  (check-equal? (string->datum  "[artemis]") 'artemis)
  (check-equal? (string->datum "   [artemis]   ") 'artemis)
  (check-equal? (string->datum " foo = bar " (cons "some test or other" 0)) '(foo . "bar") )
  (check-equal? (string->datum "name=embedded spaces OK  "
                               (cons "some test or other" 0))
                '(name . "embedded spaces OK"))
  (check-exn exn:fail:user:config-parser?
             (lambda ()
               (string->datum "spaces to the left of the = are not OK")))
  (check-exn exn:fail:user:config-parser? (lambda () (string->datum "   snorgulosity   "))))

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
  (when (zero? (run-tests all-tests 'verbose))
    (parse-every-file-on-this-box)))

(define (is-ini-file path)
  (and (file-exists? path)
       (regexp-match #px"\\.ini$" (path->string path))))

(define (parse-every-file-on-this-box)
  (pretty-print
   (fold-files
    (lambda (name flavor accumulator)
      (case flavor
        ((dir)
         (let ((desired-permissions '(read execute))
               (actual-permissions  (file-or-directory-permissions name)))
           (if (and (not (equal? name (build-path "/proc")))
                    (andmap (cut member <> actual-permissions) desired-permissions))
               accumulator
               (begin
                 (fprintf
                  (current-error-port)
                  "Avoiding ~s~%"
                  name)
                 (values accumulator #f)))))

        ((file)
         (if (is-ini-file name)
             (cons
              (cons
               name
               (with-handlers
                ([values values])
                (parse-config-ini name)))
              accumulator)
             accumulator))
        (else accumulator)
      ))
    '()
    "/"
    #f)))
