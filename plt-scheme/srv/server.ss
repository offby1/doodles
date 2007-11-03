#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0"
|#
(module server mzscheme
(require (lib "thread.ss"))

(define *tables* '())

(run-server
 1234
 (lambda (ip op)
   (file-stream-buffer-mode op 'line)
   (let loop ()
     (let ((one-datum (read ip)))
       (if (not (eof-object? one-datum))
           (begin
             (cond
              ((symbol? one-datum)
               (case one-datum
                 ((list-tables)
                  (write (cons 'tables *tables*) op))
                 (else
                  (write (cons 'unknown-command one-datum) op))))
              (else
               (write 'unknown-command op)))
             (newline op)
             (loop))
           (fprintf (current-error-port)
                    "So long suckers!~%"))
       )))
 #f)

(provide (all-defined))
)
