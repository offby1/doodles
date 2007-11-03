#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0"
|#
(module server mzscheme
(require (lib "thread.ss"))

(define *tables* '())

(fprintf (current-error-port)
         "OK, Daddy-o, lay it on me~%")

(define (dispatch one-datum)
  (cond
   ((symbol? one-datum)
    (case one-datum
      ((list-tables)
       (cons 'tables *tables*))
      (else
       (cons 'unknown-command one-datum))))
   (else
    'unknown-command)))

(run-server
 1234
 (lambda (ip op)
   (let ((client-id (let-values (((server-ip
                                   server-port
                                   client-ip
                                   client-port)
                                  (tcp-addresses ip #t)))
                      (cons client-ip client-port))))
     (file-stream-buffer-mode op 'line)
     (fprintf op
      "~s~%"
      `(welcome ,client-id))
     (let loop ()
       (let ((one-datum (read ip)))
         (if (not (eof-object? one-datum))
             (begin
               (write (dispatch one-datum) op)
               (newline op)
               (loop))
             (begin
               (close-output-port op)
               (fprintf (current-error-port)
                        "So long, ~s!~%" client-id)))))))
 #f)

(provide (all-defined))
)
