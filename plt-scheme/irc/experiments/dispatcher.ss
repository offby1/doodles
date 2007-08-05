#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e '(test/text-ui dispatcher-tests)'
|#
(module dispatcher mzscheme
(require (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2)))

(define register-handler! values)

(define dispatcher-tests

  (test-suite
   "dispatcher"
   (let ((scratchpad #f))
     (let-values (((ip op) (make-pipe)))
       (test-case
        "yow"
        (before
         (lambda ()
           (register-handler! values ip)
           (display "egads" op)
           (newline op))
         (check-equal?
          scratchpad
          'handler-ran!)))))))

(provide (all-defined))
)
