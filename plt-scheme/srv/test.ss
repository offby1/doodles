#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0"
|#

(module test mzscheme
(require (lib "thread.ss")
         "server.ss")
(define *the-port* 1234)

(define *s*
  (thread
   (lambda ()
     (fprintf (current-error-port)
              "OK, Daddy-o, lay it on me~%")
     (run-server *the-port* server-loop 1/10))))

(define (make-client)
  (call-with-values
      (lambda ()
        (let retry ((attempts 0))
          (with-handlers
              ([exn:fail:network?
                (lambda (e)
                  (fprintf (current-error-port)
                           "Darn: ~s"
                           e)
                  (when (< attempts 3)
                    (sleep 1/2)
                    (display "; retrying" (current-error-port))
                    (newline (current-error-port))
                    (retry (add1 attempts))))])
            (tcp-connect "localhost" *the-port*))))
    (lambda (ip op)
      (file-stream-buffer-mode op 'line)
      (fprintf (current-error-port)
               "New client sees greeting ~s~%"
               (read ip))
      (cons ip op))))

(define     one-client (make-client))
(define another-client (make-client))

(define (send datum client)
  (write datum (cdr client))
  (newline (cdr client))
  (fprintf (current-error-port)
           "Sent ~s; got ~s~%"
           datum
           (read (car client))))

(send 'heebie one-client)
(send 'jeebie another-client)

(send 'hang another-client)

(send 'list-tables one-client)

(sync *s*)
)