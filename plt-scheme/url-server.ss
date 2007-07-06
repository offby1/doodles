#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module url-server mzscheme
(require  (lib "url.ss" "net")
          (lib "thread.ss")
          (lib "sendurl.ss" "net")
          (only (lib "19.ss" "srfi") date->string current-date))
(define (process-lines ip func)
  (let loop ()
    (let ((line (read-line ip)))
      (when (not (eof-object? line))
        (func line)
        (loop)))))

(run-server
 7654
 (lambda (ip op)
   (process-lines
    ip
    (lambda (line)
      (let* ((u (string->url line))
             (scheme (url-scheme u)))
        (printf "~a: " (date->string (current-date) "~Y-~m-~dT~X~z"))
        (if (and scheme
                 (or
                  (string=? scheme
                            "http")
                  (string=? scheme
                            "https")))
            ;; ooh, I know how to handle this.
            (begin
              (printf "~a~%" line)
              (send-url (url->string u) #f)
              )
          ;; hmm.  Perhaps, if the scheme is #f, we should just glue
          ;; "http:" to the front and try again.
          (printf "Ain't gonna open ~s 'cuz it doesn't look like an http URL~%"
                  line)
          )
        (flush-output))))

   )
 #f ;; conn-timeout
 raise

 )
)
