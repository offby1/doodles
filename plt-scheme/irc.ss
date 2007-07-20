#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module irc mzscheme
(require (lib "async-channel.ss")
         (lib "trace.ss"))
(let-values (((ip op)
              (tcp-connect "localhost" 6667)))

  (define the-channel (make-async-channel))

  (define reader
    (thread
     (lambda ()
       (let loop ()
         (let ((line (read-line ip)))
           (if (eof-object? line)
               (printf "eof on server~%")
             (begin
               (async-channel-put the-channel line)
               (printf "<= ~s~%" line)
               (loop))))))))

  (define (put str)
    (printf "=> ~a~%" (string-append str (make-string 1 #\return)))
    (display str op)
    (display #\return op)
    (newline op)
    (flush-output op))

  (put "NICK carter")
  (put "USER erich debian irc.freenode.org :Eric Hanchrow")
  (let loop ()
    (let ((datum (async-channel-get the-channel)))
      (write datum)
      (newline)
      (loop))))
)