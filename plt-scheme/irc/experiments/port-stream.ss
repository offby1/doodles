#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e '(test/text-ui port-stream-tests)'
|#
(module port-stream mzscheme
(require (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (only (planet "port.ss" ("schematics" "port.plt" 1 0))
               port->string-list
               port->string)
         (lib "40.ss" "srfi")
         )

;; Riastradh wrote this
(define (port->line-stream ip)
  (stream-delay
   (let recur ()
     (let ((line (read-line ip)))
       (if (eof-object? line)
           stream-null
         (stream-cons line (recur)))))))

(define (line-stream->input-port s)
  (let-values (((ip op) (make-pipe #f "pipe from a line-stream")))
    (let ((driver
           (thread
            (lambda ()
              (let loop ((s s))
                (if (stream-null? s)
                    (close-output-port op)
                  (begin
                    (display (stream-car s) op)
                    (newline op)
                    (loop (stream-cdr s)))))))))
      ip)))

;; for testing
(define (split-string s)
  (port->string-list
   (line-stream->input-port
    (port->line-stream
     (open-input-string s)))))

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
                     "bar"))))

   (test-suite
    "stream->port"
    (test-equal?
     "empty"
     (split-string "") (list))
    (test-equal?
     "not quite so empty"
     (split-string "huzzah") (list "huzzah"))
    (test-equal?
     "two lines"
     (split-string "foo\nbar") (list "foo" "bar")))))

(provide (all-defined-except split-string))
)
