#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e '(test/text-ui port-stream-tests)'
|#
(module port-stream mzscheme
(require (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (only (planet "port.ss" ("schematics" "port.plt" 1 0))
               port->string-list
               port->string)
         (lib "40.ss" "srfi")
         )

(define (port->line-stream ip)
  (stream-delay
   (let recur ()
     (let ((line (read-line ip)))
       (if (eof-object? line)
           stream-null
         (stream-cons line (recur)))))))

(define (line-stream->port s)
  (open-input-string ""))

(define port-stream-tests

  (test-suite
   "big 'un"
   (test-suite
    "port->stream"
    (test-pred
     "empty"
     stream-null? (port->line-stream (open-input-string "")))
    (test-case
     "not empty"
     (let ((s  (port->line-stream (open-input-string "yow"))))
       (check-false (stream-null? s))
       (check-equal? (stream-car (port->line-stream (open-input-string "yow")))
                     "yow")
       (check-true  (stream-null? (stream-cdr s)))))
    (test-case
     "lines come back in the right order"
     (let ((two-lines (port->line-stream (open-input-string "foo\nbar"))))
       (check-equal? (stream-car two-lines)
                     "foo")
       (check-equal? (stream-car (stream-cdr two-lines))
                     "bar")))
    )
   (test-suite
    "stream->port"
    (test-equal?
     "empty"
     (port->string (line-stream->port (port->line-stream (open-input-string ""))))
     "")
    (test-equal?
     "not quite so empty"
     (port->string (line-stream->port (port->line-stream (open-input-string "huzzah"))))
     "huzzah")
    (test-equal?
     "two lines"
     (port->string-list (line-stream->port (port->line-stream (open-input-string "foo\nbar"))))
     (list "foo" "bar")))))

(provide (all-defined))
)
