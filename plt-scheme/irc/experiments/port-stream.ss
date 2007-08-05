#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e '(test/text-ui port-stream-tests)'
|#
(module port-stream mzscheme
(require (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (lib "40.ss" "srfi"))

(define (port->line-stream ip)
  (let recur ()
    (let ((line (read-line ip)))
      (if (eof-object? line)
          stream-null
        (stream-cons line (recur))))))

(define port-stream-tests

  (test-suite
   "port-stream"
   (test-pred
    "empty"
    stream-null? (port->line-stream (open-input-string "")))
   (test-false
    "not empty"
    (stream-null? (port->line-stream (open-input-string "yow"))))
   (test-equal?
    "right value in trivial case"
    (stream-car (port->line-stream (open-input-string "yow")))
    "yow")
   (test-case
    "lines come back in the right order"
    (let ((two-lines (port->line-stream (open-input-string "foo\nbar"))))
      (check-equal? (stream-car two-lines)
                    "foo")
      (check-equal? (stream-car (stream-cdr two-lines))
                    "bar")))
   ))

(provide (all-defined))
)
