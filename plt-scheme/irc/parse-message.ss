#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module parse-message mzscheme
(require (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
         (only (planet  "assert.ss"  ("offby1"     "offby1.plt")) exit-if-failed))

(define (parse-message str)
  (values "localhost." "255" "carter :I have 2 clients and 0 servers"))
(exit-if-failed
 (test/text-ui
  (test-suite
   "Evahthang"
   (test-case
    "uh ..."
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
    "er ..."
    (let ((str "NOTICE AUTH :*** No identd (auth) response")
          (expected-prefix #f)
          (expected-command "NOTICE")
          (expected-params "AUTH :*** No identd (auth) response"))
      (let-values (((actual-prefix actual-command actual-params)
                    (parse-message str)))
        (check-equal? actual-prefix expected-prefix)
        (check-equal? actual-command expected-command)
        (check-equal? actual-params expected-params))))

   )))
)