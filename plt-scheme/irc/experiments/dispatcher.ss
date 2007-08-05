#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e '(test/text-ui dispatcher-tests)'
|#
(module dispatcher mzscheme
(require (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2)))

(define register-handler! values)

(define scratchpad #f)

(define dispatcher-tests

  (test-suite
   "dispatcher"
   (test-case
    "yow"
    (let-values (((ip op) (make-pipe)))
      (register-handler!
       ip
       (lambda (string)
         (when (regexp-match
                #rx"gad"
                string)
           (set! scratchpad 'handler-ran!))))
      (display "egads" op)
      (newline op)
      (check-equal?
       scratchpad
       'handler-ran!)))))

(provide (all-defined))
)
