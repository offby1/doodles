#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module url-server mzscheme
(require  (lib "url.ss" "net")
          (lib "thread.ss")
          (lib "date.ss"))
(define (process-lines ip func)
  (let loop ()
    (let ((line (read-line ip)))
      (when (not (eof-object? line))
        (func line)
        (loop)))))

(run-server
 7655
 (lambda (ip op)
   (process-lines
    ip
    (lambda (line)
      (let* ((u (string->url line))
             (scheme (url-scheme u)))
        (printf "~a: " (date->string (seconds->date (current-seconds)) #t))
        (if (and scheme
                 (or
                  (string=? scheme
                            "http")
                  (string=? scheme
                            "https")))
            ;; ooh, I know how to handle this.
            (printf
             "Point yo' web browser at ~a~%"

             line)
          (printf "Ain't gonna open that 'cuz it doesn't look like an http URL~%")
          ))))

   )
 #f ;; conn-timeout
 )
)