#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module irc mzscheme

(let-values (((ip op)
              (tcp-connect "localhost" 6667)))

  (define (dl str)
    (printf "<= ~s~%" str))

  (define (put str)
    (printf "=> ~s~%" str)
    (display str op)
    (display #\return op)
    (newline op))

  (define (drain)
    (sync ip)
    (let loop ((result '()))
      (let ((line (read-line ip)))
        (if (or (not (char-ready? ip))
                (eof-object? line))
            (reverse result)
          (loop (cons line result))))))

  (define (drain-all)
    (for-each dl (drain)))

  (put "NICK carter")
  (drain-all)
  (put "USER erich debian irc.freenode.org :Eric Hanchrow")
  (drain-all))
)