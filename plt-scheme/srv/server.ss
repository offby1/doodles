#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0"
|#
(module server mzscheme
(require (lib "thread.ss")
         (lib "match.ss"))

(define *tables* '())

(fprintf (current-error-port)
         "OK, Daddy-o, lay it on me~%")

(define (dispatch one-datum)
  (match one-datum
   ['list-tables
    (cons 'tables *tables*)]
   [('join table)
    `(I see you would like to join table ,table)]
   [_
    (cons 'unknown-command one-datum)]))

(define server-loop
  (lambda (ip op)
    (let ((client-id (let-values (((server-ip
                                    server-port
                                    client-ip
                                    client-port)
                                   (if (tcp-port? ip)
                                       (tcp-addresses ip #t)
                                       (values #f #f #f #f))))
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
                         "So long, ~s!~%" client-id))))))))

(if #t
    (server-loop (current-input-port)
                 (current-output-port))
    (run-server 1234 server-loop #f))

(provide (all-defined))
)
