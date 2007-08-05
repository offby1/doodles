#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --load "$0"
|#

(require
 (only (planet "port.ss" ("schematics" "port.plt"))
       port->string)
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
                 (printf "splitter about to read from ~s, which has ~a bytes~%"
                         (object-name ip) (pipe-content-length ip))
                 (let ((ch (read-char ip)))
                   (printf "Splitter read ~s from ~s~%"
                           ch
                           (object-name ip))
                   (if (eof-object? ch)
                       (for-each (lambda (op)
                                   (printf "closing ~s~%"
                                           (object-name op))
                                   (close-output-port op)) ops)
                     (begin
                       (for-each
                        (lambda (op)
                          (printf "splitter about to write ~s to ~s, which has ~a bytes~%"
                                  ch
                                  (object-name op)
                                  (pipe-content-length op))
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
        (printf "1~%") (fprintf out "hey you~%")
        (printf "2~%")(check-equal? (read-line in1) "hey you")
        (printf "3~%")(check-false (char-ready? in1))
        (printf "4~%")(check-equal? (read-line in2) "hey you")
        (printf "5~%")(check-false (char-ready? in2))

        (printf "6~%")(fprintf out "get bent~%")
        (printf "7~%")(check-equal? (read-line in1) "get bent")
        (printf "8~%")(check-false (char-ready? in1) )
        (printf "9~%")(check-equal? (read-line in2) "get bent")
        (printf "10~%")(check-false (char-ready? in2))))
    )))
;;(provide (all-defined))

(test/text-ui splitter-tests)
