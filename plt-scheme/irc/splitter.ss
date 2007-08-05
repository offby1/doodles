#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --load "$0"
|#

(require
 (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
 (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
 (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2)))

(define (split-input-port ip)
  (let-values (((in1 out1) (make-pipe #f  "splitter's pipe for in1"))
               ((in2 out2) (make-pipe #f  "splitter's pipe for in2")))
    (let* ((ops (list out1 out2))
           (driver
            (thread
             (lambda ()
               (let loop ()
                 (let ((ch (read-char ip)))
                   (if (eof-object? ch)
                       (for-each (lambda (op)
                                   (close-output-port op)) ops)
                     (begin
                       (for-each
                        (lambda (op)
                          (write-char ch op))
                        ops)
                       (loop)))
                   )))
             )))
      (values in1 in2))))

(define splitter-tests

  (test-suite
   "splitter"
   (test-case
    "what gets writ to one gets writ also to the other"
    (let-values (((in out) (make-pipe #f "tests's pipe")))
      (let-values (((in1 in2) (split-input-port in)))
        (fprintf out "hey you~%")
        (check-equal? (read-line in1) "hey you")
        (check-false (char-ready? in1))
        (check-equal? (read-line in2) "hey you")
        (check-false (char-ready? in2))

        (fprintf out "get bent~%")
        (check-equal? (read-line in1) "get bent")
        (check-false (char-ready? in1) )
        (check-equal? (read-line in2) "get bent")
        (check-false (char-ready? in2))))
    )))
;;(provide (all-defined))

(test/text-ui splitter-tests)
