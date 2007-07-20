#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module irc mzscheme
(require (lib "async-channel.ss"))
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
               (loop))))))))

  (define (put str)
    (printf "=> ~s~%" str)
    (display str op)
    (display #\return op)
    (newline op))

  (define (gotsync)
    (printf "<= ~s~%" (sync the-channel)))
  (put "NICK carter")
  (gotsync)
  (put "USER erich debian irc.freenode.org :Eric Hanchrow")
  (gotsync)
  (put "JOIN #frobotzle")
  (gotsync))
)