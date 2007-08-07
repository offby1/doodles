#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module parse-tests mzscheme
(require (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2)))
(require/expose "parse-message.ss" (grab-word parse-message))

(define parse-tests
  (test-suite
   "Evahthang"
   (test-equal?
    "grab-word, just one word"
    (grab-word (open-input-string "hey"))
    "hey")
   (test-equal?
    "grab-word,  one word, trailing space"
    (grab-word (open-input-string "hey "))
    "hey")
   (test-case
    "grab-word, two words"
    (let* ((ip (open-input-string "hey  you"))
           (hey (grab-word ip))
           (you (grab-word ip)))
      (check-equal? hey "hey")
      (check-equal? you "you")))
   (test-case
    "parse-message with prefix"
    (let ((str ":localhost. 255 carter :I have 2 clients and 0 servers")
          (expected-prefix "localhost.")
          (expected-command "255")
          (expected-params "carter :I have 2 clients and 0 servers"))
      (let-values (((actual-prefix actual-command actual-params)
                    (parse-message str)))
        (check-equal? actual-prefix expected-prefix)
        (check-equal? actual-command expected-command)
        (check-equal? actual-params expected-params)))
    )
   (test-case
    "parse-message without prefix"
    (let ((str "NOTICE AUTH :*** No identd (auth) response")
          (expected-prefix #f)
          (expected-command "NOTICE")
          (expected-params "AUTH :*** No identd (auth) response"))
      (let-values (((actual-prefix actual-command actual-params)
                    (parse-message str)))
        (check-equal? actual-prefix expected-prefix)
        (check-equal? actual-command expected-command)
        (check-equal? actual-params expected-params))))

   ))
(provide parse-tests)
)