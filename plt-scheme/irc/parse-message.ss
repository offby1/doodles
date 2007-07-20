#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module parse-message mzscheme
(require (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
         (only (planet  "assert.ss"  ("offby1"     "offby1.plt")) exit-if-failed)
         (only (planet "port.ss" ("schematics" "port.plt" 1 0)) port->string))

(define (grab-word ip)
  (let loop ((chars '()))
    (let ((ch (read-char ip)))
      (cond
       ((or (eof-object? ch)
            (char=? #\space ch))
        (apply string (reverse chars)))
       (else
        (loop (cons ch chars)))))))

(define (parse-message str)
  (let ((prefix #f)
        (command #f)
        (params #f))
    (let ((ip (open-input-string str)))
      (when (char=? #\: (peek-char ip))
        (read-char ip)
        (set! prefix (grab-word ip)))
      (set! command (grab-word ip))
      (set! params (port->string ip))
      (values prefix command params)))
  )

(exit-if-failed
 (test/text-ui
  (test-suite
   "Evahthang"
   (test-equal?
    "grab-word"
    (grab-word (open-input-string "hey"))
    "hey")
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

   )))
)